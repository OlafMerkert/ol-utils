(in-package #:ol-utils)

(export '(while
          until
          neither))

(defmacro while (condition &body body)
  "Execute BODY repeatedly as long as CONDITION holds."
  `(do ()
       ((not ,condition))
     ,@body))

(defmacro until (condition &body body)
  "Execute BODY repeatedly until CONDITION holds."
  `(do ()
       (,condition)
     ,@body))

(defmacro neither (&rest conds)
  "None (of CONDS) shall be true."
  `(and ,@(mapcar #`(not ,a1) conds)))


(defmacro! in (o!item list &key (test 'eql))
  "The macro version of member.  As TEST is simply spliced in, it
ought to be a symbol."
  `(or ,@(mapcar #`(,test ,g!item ,a1) list)))
