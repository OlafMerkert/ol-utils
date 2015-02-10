(in-package #:ol-utils)

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
        (every test seq1 seq2))))

(defun remove* (list sequence &key test)
  "Remove all elements of `list' from `sequence'."
  (if (null list)
      sequence
      (remove* (cdr list) (remove (car list) sequence :test test) :test test)))

(defmacro! define-trimmer (name args trim-position &key include documentation)
  "Define a trim function, which cuts of the start (or end) of `sequence'
up to (or including) the computed `trim-position'. This macro is
anaphoric, you should put `sequence' among the `args'."
  `(defun ,name (,@args &key from-end)
     (let ((,g!pos (,@trim-position :from-end from-end)))
       (cond ((and from-end ,g!pos)
              (subseq sequence 0 ,(if include g!pos `(+ 1 ,g!pos))))
             (,g!pos (subseq sequence ,(if include `(+ 1 ,g!pos) g!pos)))
             (t sequence)))))

(define-trimmer trim1-if (test sequence)
  (position-if-not test sequence)
  :documentation "Remove all entries satisfying `test' from the start (respectively
end) of `sequence'.")

(define-trimmer trim1-if-not (test sequence)
  (position-if test sequence)
  :documentation "Remove all entries not satisfying `test' from the start (respectively
end) of `sequence'.")

(define-trimmer trim1 (elt sequence)
  (position elt sequence)
  :include t
  :documentation "Remove everything from `sequence' up to and including the first
occurrence of `elt'. Think `zap-to-char' of Emacs.")




