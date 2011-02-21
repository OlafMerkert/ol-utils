(in-package #:ol-utils)

(export '(swallow list->gensyms
          memoize mlambda
          defmemfun))

(defun swallow (fun)
  "Transform the function such that it appears to accept arguments,
but call it with none."
  (lambda (&rest x)
    (declare (ignore x))
    (funcall fun)))

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
