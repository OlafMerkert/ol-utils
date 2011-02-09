(in-package #:ol-utils)

(export '(swallow list->gensyms))

(defun swallow (fun)
  "Transform the function such that it appears to accept arguments,
but call it with none."
  (lambda (&rest x)
    (declare (ignore x))
    (funcall fun)))

(defun list->gensyms (&rest lists)
  "Collect a gensym for every common element of the lists."
  (apply #'mapcar (swallow #'gensym) lists))