(in-package #:ol-utils)
(olr)

(defmacro while% (condition &body body)
  "Execute BODY repeatedly as long as CONDITION holds."
  `(do ()
       ((not ,condition))
     ,@body))

(defmacro until% (condition &body body)
  "Execute BODY repeatedly until CONDITION holds."
  `(do ()
       (,condition)
     ,@body))

(defmacro neither% (&rest conds)
  "None (of CONDS) shall be true."
  `(and ,@(mapcar #`(not ,a1) conds)))

(defmacro! until-t (expr)
  "Evaluate expression repeatedly until it return non-NIL."
  `(do ((,g!result ,expr ,expr))
       (,g!result)))

(defmacro! in (o!item list &key (test 'eql))
  "The macro version of member.  As TEST is simply spliced in, it
ought to be a symbol."
  `(or ,@(mapcar #`(,test ,g!item ,a1) list)))

;; todo is this the right place?
(defmacro! pass-symbol ((op o!arg))
  "Only apply a univariate function `op', if the actual `arg' is not a
symbol. For symbols, just pass them."
  `(if (symbolp ,g!arg)
       ,g!arg
       (,op ,g!arg)))
