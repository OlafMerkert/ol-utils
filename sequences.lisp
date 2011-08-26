(in-package #:ol-utils)

(export '(begins-with
          copy))

(defun begins-with (seq start)
  "Test whether the first part of SEQ is START."
  (let ((l (length start)))
    (and (<= l (length seq))
         (equal (subseq seq 0 (length start))
                start))))

(defun copy (result-type sequence)
  "Create a copy of SEQUENCE that has type RESULT-TYPE."
  (map result-type #'identity sequence))

