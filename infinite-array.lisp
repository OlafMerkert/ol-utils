(in-package :ol-utils)
(olr)

;;; some functions to treat the usual array structure transparently as
;;; infinite. Unlike with lazy-array, there is no auto-filling, the
;;; main point is being able to always do (setf (aref a n) v) without
;;; having to worry whether n is below the array length.

(defsymconstant +unbound-element+)

(defun make-infinite-array (&optional (initial-length 10))
  (make-array initial-length :initial-element +unbound-element+
              :adjustable t))

(defun inf-aref (array index)
  (if (>= index (length array))
      #1=(error "Unbound element of array")
      (let ((element (aref array index)))
        (if (eq element +unbound-element+)
            #1#
            element))))

(defun extend-infinite-array (array index)
  (unless (< index (length array))
    (let ((adj-array
           (adjust-array array (max (+ index 10)
                                    (array-dimension array 0)))))
      (unless (eq array adj-array)
        ;; TODO allow recovering from this
        (error "supposed infinite ARRAY was not adjustable."))
      adj-array)))


(defun (setf inf-aref) (value array index)
  (extend-infinite-array array index)
  (setf (aref array index) value))

(defun ignore-unbound (fn)
  "Helper function to use the ordinary map with infinite-array"
  (lambda (x)
    (if (eq x +unbound-element+)
        x
        (funcall fn x))))

(defun inf-array-map (fn inf-array)
  "Map over an infinite array, by ignoring unbound elements. This does
change indices."
  (let ((new-index -1)
        (new-array (make-infinite-array)))
    (map nil (lambda (x)
               (unless (eq x +unbound-element+)
                 (setf (inf-aref new-array (incf new-index))
                       (funcall fn x))))
         inf-array)
    new-array))
