(in-package :ol-utils)

(export '(lazy-aref
          make-lazy-array
          this
          index))

;;; lazy array datastructure, developed for power series computations

(defstruct (lazy-array (:constructor make-lazy-array%))
  (array (make-array 10 :adjustable t :fill-pointer 0))
  (function (ilambda (this i) nil))
  (finite nil)
  (default-value))

(defun lazy-aref (lazy-array index)
  "Give the appropriate entry of the lazy array.  If the entry has not
been calculated, all the missing entries up to it are calculated and
appended to"
  (if (aand (lazy-array-finite lazy-array) (>= index it))
      ;; when over the finite bound, just return the default value, do not extend the array.
      (lazy-array-default-value lazy-array)
      (let ((array (lazy-array-array lazy-array)))
        (unless (< index (length array))
          ;; generate the missing ones
          ;; TODO maybe call adjust array
          (loop for i from (length array) to index do
               (vector-push-extend
                (funcall (lazy-array-function lazy-array) array i)
                array)))
        (aref array index))))

(defmacro make-lazy-array ((&key start (index-var 'index) finite default-value)
                           &body fill-form)
  "START must be a list of the first entries of the array.  In
FILL-FORM, the generated actual array is bound to THIS, so you may
reference array elements of lower index, but never of higher.  The
index of the field to be filled is stored in a variable whose name you
provide with INDEX-VAR, by default just INDEX."
  `(make-lazy-array%
    :array (make-array ,(length start) :adjustable t :fill-pointer t
                       :initial-contents (list ,@start))
    :function (ilambda (this ,index-var)
                ,@fill-form)
    :finite ,finite
    :default-value ,default-value))

(defun la% (default &rest start)
  "Abbreviation for lazy arrays that are guaranteed finite.  If only
one parameter of type array is given, use that one to base the
lazy-array on.  Sets the default-value to 0."
  (when (and (length=1 start)
             (arrayp (first start)))
    (setf start (first start)))
  (make-lazy-array% :array (make-array (length start)
                                       :adjustable t :fill-pointer t
                                       :initial-contents start)
                    :finite (length start)
                    :default-value default))

(defun la (&rest start)
  "Abbreviation for lazy arrays that are guaranteed finite.  If only
one parameter of type array is given, use that one to base the
lazy-array on.  Sets the default-value to 0."
  (apply #'la% nil start))

;; manipulation of lazy arrays
(defun lazy-array-take (lazy-array n &optional (lazy-p t))
  "Extract the first N entries from the LAZY-ARRAY.  If LAZY-P is nil,
the result will be an ordinary array."
  ;; first evaluate all of these
  ;; TODO what if the lazy-array is finite?? then lazy-aref won't work as expected here
  (lazy-aref lazy-array n)
  (let ((result (subseq (lazy-array-array lazy-array) 0 n)))
    (if lazy-p
        (la (lazy-array-default-value lazy-array) result)
        result)))

(defmacro la-finite-test (lazy-arrays &body body)
  "If all the given LAZY-ARRAYS are finite, then evaluate the body
  expression where each array stands for its FINITE slot."
  `(let ,(mapcar #`(,a1 (lazy-array-finite ,a1)) lazy-arrays)
     (when (and ,@lazy-arrays)
       ,@body)))

(defun lazy-array-drop (lazy-array n)
  "Return a new lazy array with the first n entries removed."
  ;; this is rather tricky, as it has to work when the generation
  ;; function depends on entries which this function removes from the
  ;; array.  A simple solution is to always refer to the original
  ;; array (maybe not the most efficient one).
  (make-lazy-array (:start nil :finite (la-finite-test (lazy-array)
                                                   (- lazy-array n)))
    (lazy-aref lazy-array (+ index n))))
