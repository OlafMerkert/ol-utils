(in-package #:ol-utils)

(export '(swallow ilambda ilambda+
          list->gensyms
          memoize mlambda
          defmemfun
          compose compose/red))

(ew
 (def-symbol-p &)

 (defun args->names (args)
   "Extract the parameter names from an argument list."
   (mapcan (lambda (x)
             (cond ((&-symbol-p x) nil)
                   ((listp x) (list (car x)))
                   (t (list x))))
           args)))

(defmacro ilambda (args &body body)
  "lambda form where all arguments are declared ignorable."
  `(lambda ,args
     (declare (ignorable ,@(args->names args)))
     ,@body))

(defmacro ilambda+ (&body body)
  "lambda form where any arguments are accepted, and anaphorically bound to
  ARGS."
  ;; this provides almost identical functionality as mswallow
  `(ilambda (&rest args)
     ,@body))

(defun swallow (fun)
  "Transform the function such that it appears to accept arguments,
but call it with none."
  (ilambda (&rest x)
    (funcall fun)))

(defun list->gensyms (&rest lists)
  "Collect a gensym for every common element of the lists, if the
first item is a keyword symbol use it as the base of the gensyms."
  (if (keywordp (first lists))
      (apply #'mapcar (ilambda+ (gensym (symbol-name (first lists)))) (rest lists))
      (apply #'mapcar (swallow #'gensym) lists)))


;;; Memoization
(defun memoize (fun)
  "Make the given function memoizing."
  (let ((memo (make-hash-table :test 'equal)))
    (lambda (&rest args)
      (multiple-value-bind (res found-p) (gethash args memo)
        (if found-p
            res
            (setf (gethash args memo)
                  (apply fun args)))))))

(defmacro/g! mlambda (args &body body)
  "Anaphoric lambda that does memoization."
  `(let (,g!funo)
     (labels ((,g!fun (,@args) ,@body)
              (self (&rest ,g!args) (apply ,g!funo ,g!args)))
       (setf ,g!funo (memoize #',g!fun))
       #'self)))

(defmacro/g! defmemfun (name args &body body)
  "Define a memoizing function.  Allows recursive calls as well."
  (let ((names (args->names args)))
    `(let (,g!funo)
       (labels ((,g!fun (,@names)
                  (symbol-macrolet ((,name ',g!self))
                    ,@body))
                (,g!self (&rest ,g!args) (apply ,g!funo ,g!args)))
         (setf ,g!funo (memoize #',g!fun))
         (defun ,name (,@args)
           (funcall ,g!funo ,@names))))))

(defun compose/red (&rest functions)
  "Compose the functions.  The leftmost function will be applied last.
Currently only works with functions that take a single argument."
    (lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defmacro! compose (&rest functions)
  "Compose the functions.  The leftmost function will be applied
  last."
  (labels ((generate-funcalls (functions value call)
             (if functions
                 (generate-funcalls (rest functions)
                                    `(,call ,(first functions)
                                              ,value)
                                    'funcall)
                 value)))
  `(lambda (&rest ,g!x)
     ,(generate-funcalls (reverse functions) g!x 'apply))))
