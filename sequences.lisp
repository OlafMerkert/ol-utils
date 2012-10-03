(in-package #:ol-utils)

(export '(begins-with
          copy))

(defun begins-with (seq start &key (test 'equal))
  "Test whether the first part of SEQ is START. TEST must be a
predicate comparing sequences."
  (let ((l (length start)))
    (and (<= l (length seq))
         (funcall test
                  (subseq seq 0 (length start))
                  start))))

(defun copy (result-type sequence)
  "Create a copy of SEQUENCE that has type RESULT-TYPE."
  (map result-type #'identity sequence))

