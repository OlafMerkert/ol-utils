(in-package :ol-utils)
(olr)

(defalias mvbind multiple-value-bind (vars value-form &body body))

(defalias dbind destructuring-bind (lambda-list expression &body body))

(defalias mvprog1 multiple-value-prog1)

(defmacro let1 (bind &body body)
  "Shortcut for `let' if one wants to introduce only one variable
binding. Example `(le1 (var value) ...)'."
  `(let (,bind) ,@body))

(defalias le1 let1 (bind &body body))

(defmacro labels1 (definition &body body)
  `(labels (,definition) ,@body))

(defmacro macrolet1 (definition &body body)
  `(macrolet (,definition) ,@body))

;;; modify macros
(define-modify-macro appendf (list) append)
