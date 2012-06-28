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
