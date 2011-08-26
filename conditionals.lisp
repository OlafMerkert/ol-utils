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