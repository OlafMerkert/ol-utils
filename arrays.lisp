(in-package #:ol-utils)

(export '(list->array array->list
          fill-array% fill-array old indices index
          shuffle
          arange amrange
          alast))

(defun list->array (list)
  "Make an array from list."
  (coerce list 'vector))

;; TODO Mehrdimensionale Arrays
(defun array->list (array)
  "Make a list from the 1-dim array."
  (coerce array 'list))

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
expr.  fill-spec consists of integers or the keyword :fill.

An integer selects a single row/col in the matching dimension,
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
  `(destructuring-bind ,(mklist names) index
     (declare (ignorable ,@(mklist names)))
     ,@body))

(defun shuffle (seq)
  "Destructively shuffle the given sequence."
  (let ((n (length seq)))
    (iter (for i from (- n 1) downto 1)
          (for j = (random (+ i 1)))
          (rotatef (elt seq i)
                   (elt seq j))))
  seq)

(defun arange (start &optional end (step 1))
  "As range, but builds an array/vector instead of a list."
  (coerce (range start end step) 'vector))

(defun amrange (start &optional end (step 1))
  (coerce (mrange start end step) 'vector))

(defun alast (vector)
  "Get at the last element of a vector."
  (aref vector (1- (length vector))))

(defsetf alast (vector) (value)
  (with-gensyms!
     `(let ((,g!vector ,vector))
        (setf (aref ,g!vector (1- (length ,g!vector)))
              ,value))))

(defmacro! ensure-adjustable-array (o!place)
  "Transform the sequence held in place to an adjustable array. This
macro can be useful with arrays taken from a prevalence store."
  `(unless (and (arrayp ,g!place)
                (adjustable-array-p ,g!place))
    (let ((,g!array (make-array (length ,g!place) :adjustable t :fill-pointer t)))
      (map-into ,g!array #'identity ,g!place)
      (setf ,o!place ,g!array))))
