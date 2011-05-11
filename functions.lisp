(in-package #:ol-utils)

(export '(swallow mswallow ilambda
          list->gensyms
          memoize mlambda
          defmemfun
          compose composer))

(defun swallow (fun)
  "Transform the function such that it appears to accept arguments,
but call it with none."
  (lambda (&rest x)
    (declare (ignore x))
    (funcall fun)))

(defmacro/g! mswallow (&body body)
  "Enclose the body with a lambda that ignores arbitrary arguments."
  `(lambda (&rest ,g!args)
     (declare (ignore ,g!args))
     ,@body))

(defmacro ilambda (args &body body)
  "lambda form where all arguments are declared ignorable."
  `(lambda ,args
     (declare (ignorable ,@(args->names args)))
     ,@body))

(defun list->gensyms (&rest lists)
  "Collect a gensym for every common element of the lists."
  (apply #'mapcar (swallow #'gensym) lists))


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


(def-symbol-p &)

(defun args->names (args)
  "Extract the parameter names from an argument list."
  (mapcan (lambda (x)
            (cond ((&-symbol-p x) nil)
                  ((listp x) (list (car x)))
                  (t (list x))))
          args))

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

(defun compose (&rest functions)
  "Compose the functions.  The leftmost function will be applied last.
Currently only works with functions that take a single argument."
    (lambda (x)
      (reduce #'funcall functions
              :initial-value x
              :from-end t)))

(defmacro/g! composer (&rest function-symbols)
  "Compose the functions given by symbols.  The leftmost function will
be appliead last.  Currently expects all functions to take a single
argument."
  (labels ((call (fns)
             (if fns
                 `(,(car fns) ,(call (cdr fns)))
                 g!arg)))
   `(lambda (,g!arg)
      ,(call function-symbols))))
