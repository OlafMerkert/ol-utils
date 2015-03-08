(in-package #:ol-utils)

(defun list->array (list)
  "Make an array from list."
  (coerce list 'vector))

;; TODO Mehrdimensionale Arrays
(defun array->list (array)
  "Make a list from the 1-dim array."
  (coerce array 'list))

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
    (let ((,g!array (make-array (length ,g!place) :adjustable t)))
      (map-into ,g!array #'identity ,g!place)
      (setf ,o!place ,g!array))))

(defun multi-dim-dotimes+ (index-function positions)
  "a helper function for FILL-ARRAY."
  ;; just reverse the positions once here, so we don't have to reverse
  ;; the index-lists all the time.
  (setf positions (reverse positions))
  (labels ((index-range (position)
             ;; normalise the range information
             (if (listp position)
                 (values (first position) (second position))
                 (values 0 position)))
           (rec (positions indices)
             (if positions
                 ;; more ranges to iterate over
                 (multiple-value-bind (start end) (index-range (first positions))
                   (dotimes+ (i start end)
                       ((rest (rest positions)))
                     (rec rest
                          (cons i indices))))
                 ;; all index information available
                 (funcall index-function indices))))
    (rec positions nil)))


(defun fill-array (array fill-function &optional (positions nil positions?))
  "fill the ARRAY with the FILL-FUNCTION in the places given by
POSITIONS. POSITIONS is a list of index ranges, where an index range
is either a tuple (start end) with inclusive start and exclusive end,
or simply an integer end, equivalent to (0 end). FILL-FUNCTION will be
called in the same way as aref--first argument is the array, the
remaining are the indices. "
  (unless positions?
    (setf positions (array-dimensions array)))
  ;; return the now filled array
  (multi-dim-dotimes+
   (lambda (indices)
     (setf (apply #'aref        array indices)
           (apply fill-function array indices)))
   positions)
  array)

(defun make-array/fill% (dimensions fill-function)
  (aprog1 (make-array dimensions)
    (fill-array it fill-function dimensions)))

(defmacro make-array/fill (dimensions indices &body fill-expr)
  `(make-array/fill% (list ,@(mklist dimensions))
                     (ilambda (this ,@indices)
                       ,@fill-expr)))


(defmacro! make-array/sparse (dimensions default-value &body special-values)
  `(let ((,g!array (make-array (list ,@(mklist dimensions)) :initial-element ,default-value)))
     ,@(mapcar #`(setf (aref ,g!array ,@(mklist (first a1)))
                       ,(second a1))
               special-values)
     ,g!array))

(defun map-array (function array &rest other-arrays)
  "Map a `function' over all entries of `array', and `other-arrays'.
Please note that the `other-arrays' must be at least as big as
`array'."
  (make-array/fill% (array-dimensions array)
                    (ilambda (this &rest indices)
                      (apply function #1=(apply #'aref array indices)
                             (mapcar (lambda (array) #1#) other-arrays)))))

(defun map-array1 (function array)
  "Map a `function' over all entries of `array'."
  (make-array/fill% (array-dimensions array)
                    (ilambda (this &rest indices)
                      (funcall function #1=(apply #'aref array indices)))))



(defun fill-array/old% (fill-spec array fill-function)
  (multi-dim-dotimes+ (lambda (indices)
                        (setf #1=(apply #'aref array indices)
                              (funcall fill-function #1# indices)))
                      (mapcar (lambda (x d)
                                (case x
                                  (:fill (list 0 d))
                                  (t (list x (+ x 1)))))
                              fill-spec (array-dimensions array)))
  array)

(defmacro! fill-array/old (fill-spec o!array expr)
  "Fill the array in the parts described by fill-spec with values described by
expr.  fill-spec consists of integers or the keyword :fill.

An integer selects a single row/col in the matching dimension,
whereas :fill selects all rows/cols in the matching dimension.

The expr can use the anaphoric variables old and index to access the
previous value of the field and the list of its indices."
  `(fill-array/old% (list ,@fill-spec) ,g!array
                    (lambda (old index)
                      (declare (ignorable old index))
                      ,expr)))

(defun pad-vector-front (vector required-length &optional (initial-element 0))
  "add `initial-element' to the front of `vector', until it has `required-length'."
  (let ((n (length vector))
        (new-vector (make-array required-length :initial-element initial-element)))
    (when (> n required-length)
      (error "vector is already too long, has length ~A when ~A is required." n required-length))
    (iter (for i from (- required-length n))
          (for el in-vector vector)
          (setf (aref new-vector i) el))
    new-vector))

(defun map-vector-2 (vector-a vector-b function-2
                     &optional (function-a 'identity) (function-b function-a))
  "For the vectors of different length, produce a new vector with
  length the maximal length, for indices where both vectors have
  entries, use `function-2' to combine them. If the vectors have
  different length, map the remaining entries (of one vector) through
  the according function. Note we cannot guarantee in which order the
  functions are applied on the entries."
  (let (length
        result-vector
        (l-a (length vector-a))
        (l-b (length vector-b)))
    (cond ((= l-a l-b)
           (setf length l-a
                 result-vector (make-array l-a)))
          ((< l-a l-b)
           (setf length l-a
                 result-vector (make-array l-b))
           (iter (for i from l-a below l-b)
                 (setf (svref result-vector i)
                       (funcall function-b (svref vector-b i)))))
          ((> l-a l-b)
           (setf length l-b
                 result-vector (make-array l-a))
           (iter (for i from l-b below l-a)
                 (setf (svref result-vector i)
                       (funcall function-a (svref vector-a i))))))
    (iter (for i from 0 below length)
          (setf (svref result-vector i)
                (funcall function-2 (svref vector-a i) (svref vector-b i))))
    result-vector))
