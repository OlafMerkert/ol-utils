(in-package #:ol-utils)

(export '(sum prod
          cumulative-sums))

;; TODO capture the abstraction
(defmacro! sum ((var start &optional end (step 1)) expr)
  `(do-range (,var ,start ,end ,step ,g!sum)
       ((,g!sum 0))
     (incf ,g!sum ,expr)))

(defmacro! prod ((var start &optional end (step 1)) expr)
  `(do-range (,var ,start ,end ,step ,g!prod)
       ((,g!prod 1))
     (setf ,g!prod (* ,g!prod ,expr))))

(defun cumulative-sums (seq &optional (start 0))
  "Calculate the cumulative sums of seq."
  (dotimes (i (length seq))
    (setf (elt seq i)
          (incf start (elt seq i))))
  seq)
