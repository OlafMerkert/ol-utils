(in-package #:ol-utils)

(export '(starts-with))

(defun starts-with (start seq)
  "Test whether the first part of seq is start."
  (let ((l (length start)))
    (and (<= l (length seq))
         (equal (subseq seq 0 (length start))
                start))))
