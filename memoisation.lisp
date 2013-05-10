(in-package :ol-utils)

;;; Memoization
(defparameter initial-memoisation-size 1000)

(defsymconstant +uncalculated+)
(defsymconstant +memo-clear+)
(defsymconstant +memo-container+)

(defun make-memo-container/hash-table ()
  (declare (ignore arity))
  (make-hash-table :test 'equal :size initial-memoisation-size))

(defun get-memo/hash-table (container function args)
  (multiple-value-bind (result found-p) #1=(gethash args container)
    (if found-p
        result
        (setf #1# (apply function args)))))

(defun memo-clear/hash-table (container)
  (clrhash container))

(defun make-memo-container/array ()
  (make-array initial-memoisation-size
              :initial-element +uncalculated+
              :adjustable t))

(defun get-memo/array (container function args)
  ;; careful, we are only memoising the first parameter, so the
  ;; function may have more, but these will only be relevant at the
  ;; first invocation.
  (let ((n (first args)))
    (cond ((or (not (integerp n)) (minusp n))
           (error "invalid parameter type, expected non-negative integer."))
          ((< (length container) n)
           (adjust-array container (+ n 100) :initial-element +uncalculated+)
           #1=(setf (aref container n)
                    (apply function args)))
          ((eq (aref container n) +uncalculated+)
           #1#)
          (t (aref container n)))))

(defun memo-clear/array (container)
  (dotimes (i (length container))
    (setf (aref container i) +uncalculated+)))

(bind-multi ((memoize memoize/hash-table memoize/array)
             (make-memo-container make-memo-container/hash-table make-memo-container/array)
             (get-memo get-memo/hash-table get-memo/array)
             (memo-clear memo-clear/hash-table memo-clear/array)
             (doc
              "Make the given function memoizing on all parameters, which are
compared using EQUAL."
              "Make the given function memoizing on the first argument, which is
expected to always be a non-negative integer." ))
 (defun memoize (function)
   doc
   (let ((memo (make-memo-container)))
     (lambda (&rest args)
       ;; special constants allow to get at the container, and also
       ;; allow clearing everything.
       (case (first args)
         (+memo-clear+ (memo-clear memo))
         (+memo-container+ memo)
         (t
          (get-memo memo function args)))))))

;;; the memoisation macros
(bind-multi ((memolabels memolabels memolabels/int+)
             (memoize memoize/hash-table memoize/array)
             (memolambda memolambda memolambda/int+)
             (memodefun memodefun memodefun/int+))
  (defmacro! memolabels (definitions &body body)
    "Like labels, but all function are memoising."
    (let ((function-vars (list->gensyms definitions)))
      `(let ,function-vars
         (flet ,(mapcar #2`(,(first a1) (&rest ,g!args) (apply ,a2 ,g!args))
                        definitions function-vars)
           (setf ,@(mapcan #2`(,a2 (memoize/hash-table (lambda ,@(rest a1))))
                           definitions function-vars))
           ,@body))))

  (defmacro memolambda (args &body body)
    "Create a memoising lambda function, that may recursively invoke itself
with the anaphoric variable SELF."
    `(memolabels ((self ,args ,@body))
       #'self))

  (defmacro! memodefun (name args &body body)
    "Define a global memoising function."
    `(let ((,g!memofun (memoize/hash-table (lambda ,args ,@body))))
       (defun ,name (&rest ,g!args)
         (apply ,g!memofun ,g!args)))))
