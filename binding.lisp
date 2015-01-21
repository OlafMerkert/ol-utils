(in-package :ol-utils)

(defalias mvbind multiple-value-bind (vars value-form &body body))

(defalias dbind destructuring-bind (lambda-list expression &body body))

(defalias mvprog1 multiple-value-prog1)

(defmacro le1 (bind &body body)
  "Shortcut for `let' if one wants to introduce only one variable
binding. Example `(le1 (var value) ...)'."
  `(let (,bind) ,@body))

;;; modify macros
(define-modify-macro appendf (list) append)
