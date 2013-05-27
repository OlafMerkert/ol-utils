(in-package #:ol-utils)

(export '(swallow ilambda ilambda+
          list->gensyms
          memoize mlambda
          defmemfun
          compose compose/red))

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

(def-symbol-p x!)

(defmacro clambda (function &rest arguments)
  "Create a function from a named function where parameters are fixed.
ATM, only positional parameters are supported, and non-fixed
parameters must start with \"x!\"."
  ;; TODO support for &optional etc
  (let ((free-variables (remove-if-not #'x!-symbol-p arguments )))
    `(lambda ,free-variables
       (,function ,@arguments))))
