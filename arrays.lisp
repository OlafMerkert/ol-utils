(in-package #:ol-utils)

(export '(list->array array->list
          fill-array% fill-array old index))

(defun list->array (list)
  "Make an array from list."
  (let ((n (length list)))
    (values
     (make-array n :initial-contents list)
     n)))

;; TODO Mehrdimensionale Arrays
(defun array->list (array)
  "Make a list from the 1-dim array."
  (loop for x across array collect x))

(defun fill-array% (dim index fill-spec array fill-fn)
  "Helper function to fill-array."
  (cond ((null fill-spec)            ; all indizes have been specified
         (let ((index (reverse index)))
           ;; call fill-fn for the index and put its value in the array
           (setf #1=(apply #'aref array index)
                 (funcall fill-fn #1# index))))
        ((eq :fill (car fill-spec)) ; :fill means fill along this dimension
         (dotimes (i (array-dimension array dim))
           (fill-array% (1+ dim) (cons i index)
                        (cdr fill-spec) array fill-fn)))
        ;; otherwise the index is already fixed
        (t (fill-array% (1+ dim) (cons (car fill-spec) index)
                        (cdr fill-spec) array fill-fn))))

(defmacro! fill-array (fill-spec o!array expr)
  "Fill the array in the parts described by fill-spec with values described by
expr.fill-spec consists of integers or the keyword :fill.

The integers select a single row/col in the matching dimension,
whereas :fill selects all rows/cols in the matching dimension.

The expr can use the anaphoric variables old and index to access the
previous value of the field and the list of its indices."
  `(progn
     (fill-array% 0 nil (list ,@fill-spec) ,g!array
               (lambda (old index)
                 (declare (ignorable old index))
                 ,expr))
     ,g!array))

(defmacro indices (names &body body)
  "Companion macro for fill-array, allowing to name the indices more
conveniently."
  `(destructuring-bind ,(mklist names) index ,@body))
