(in-package #:ol-utils)

(export '(sum prod
          cumulative-sums))

;; TODO capture the abstraction
(defmacro! sum ((var start &optional end (step 1)) expr)
  (error "how about using iterate for summation?"))

(defmacro! prod ((var start &optional end (step 1)) expr)
  (error "how about using iterate for multiplication?"))

(defun cumulative-sums (seq &optional (start 0))
  "Calculate the cumulative sums of seq."
  (dotimes (i (length seq))
    (setf (elt seq i)
          (incf start (elt seq i))))
  seq)

(defun square-multiply (base exponent multiplication)
  "Calculate BASE^EXPONENT as it arises from the binary operation
  MULTIPLICATION."
  (unless (and (integerp exponent)
               (> exponent 0))
    (error
     "Square-Multiply requires a positive integer exponent, but got ~A."
     exponent))
  (iter (for i from (1- (integer-length exponent)) downto 0)
        (for result first base then (funcall multiplication result result))
        (if-first-time nil
                       (if (logbitp i exponent)
                           (setf result (funcall multiplication result base))))
        (finally (return result))))
