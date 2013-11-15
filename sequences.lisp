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

(defun sequence= (seq1 seq2 &key (start 0) end (test #'equal))
  "Check that two sequences agree completely on a subsequence. Type of
sequences is irrelevant, elements are compared using `test', by
default `equal'."
  (let ((seq1 (subseq seq1 start end))
        (seq2 (subseq seq2 start end)))
   (and (= (length seq1) (length seq2))
        (every (lambda (s1 s2) (funcall test s1 s2)) seq1 seq2))))
