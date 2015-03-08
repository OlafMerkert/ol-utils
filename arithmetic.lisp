(in-package #:ol-utils)


;; TODO capture the abstraction for sum and prod

(defun cumulative-sums (seq &optional (start 0))
  "Calculate the cumulative sums of seq."
  (dotimes (i (length seq))
    (setf (elt seq i)
          (incf start (elt seq i))))
  seq)

(defun square-multiply (base exponent multiplication
                        &optional squaring)
  "Calculate BASE^EXPONENT as it arises from the binary operation
  MULTIPLICATION."
  (when squaring
    (warn "SQUARE-MULTIPLY: does not use `squaring' parameter anymore."))
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

(defalias ^ expt (base power))

(defalias _ elt (seq index))

(defmacro! signcase (o!number minus zero plus &optional (nonumber nil noerror-p))
  "Given a real `number', check for sign and branch to the appropriate
case. NIL is also considered as zero. If `nonumber' is unspecified,
signal an error if we did not get NIl or a `real'."
  `(cond ((and ,g!number (not (realp ,g!number)))
          ,(if noerror-p nonumber `(error "Expected number in signcase, got ~A" ,g!number)))
         ((or (not ,g!number) (zerop ,g!number)) ,zero)
         ((plusp ,g!number) ,plus)
         (t ,minus)))
