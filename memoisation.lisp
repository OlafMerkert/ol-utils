(in-package :ol-utils)

;;; Memoization
(defparameter initial-memoisation-size 1000)

(defsymconstant +uncalculated+)
(defsymconstant +memo-clear+)
(defsymconstant +memo-container+)

(defun make-memo-container/hash-table ()
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

(bind-multi ((memoize memoize memoize/i)
             (make-memo-container make-memo-container/hash-table make-memo-container/array)
             (get-memo get-memo/hash-table get-memo/array)
             (memo-clear memo-clear/hash-table memo-clear/array)
             (doc
              "Make the given function memoizing on all parameters, which are
compared using `equal'."
              "Make the given function memoizing on the first argument, which is
expected to always be a non-negative integer." ))
  (defun memoize (function)
    doc
    (let ((memo (make-memo-container)))
      (lambda (&rest args)
        ;; special constants allow to get at the container, and also
        ;; allow clearing everything.
        (cond
          ((eq (first args) +memo-clear+) (memo-clear memo))
          ((eq (first args) +memo-container+) memo)
          (t
           (get-memo memo function args)))))))

;;; the memoisation macros
(bind-multi ((memolabels memolabels memolabels/i)
             (memoize memoize memoize/i)
             (memolambda memolambda memolambda/i)
             (memodefun memodefun memodefun/i))
  (defmacro! memolabels (definitions &body body)
    "Like labels, but all function are memoising."
    (let ((function-vars (list->gensyms definitions)))
      `(let ,function-vars
         (flet ,(mapcar #2`(,(first a1) (&rest ,g!args) (apply ,a2 ,g!args))
                        definitions function-vars)
           (setf ,@(mapcan #2`(,a2 (memoize (lambda ,@(rest a1))))
                           definitions function-vars))
           ,@body))))

  (defmacro memolambda (args &body body)
    "Create a memoising lambda function, that may recursively invoke itself
with the anaphoric variable SELF."
    `(memolabels ((self ,args ,@body))
       #'self))

  (defmacro! memodefun (name args &body body)
    "Define a global memoising function."
    `(let ((,g!memofun (memoize (lambda ,args ,@body))))
    ;; todo use args for debugging purposes
       (defun ,name (&rest ,g!args)
         (apply ,g!memofun ,g!args)))))

;;; add facilities to change the "values" of memoised functions
(defun set-memo (function value &rest args)
  (let ((container (funcall function +memo-container+))
        (n (first args)))
    (etypecase container
      (hash-table (setf (gethash args container) value))
      (array (setf (aref container n) value)))
    value))

(defsetf mfuncall (function &rest args) (value)
  `(set-memo ,function ,value ,@args))

(defun memoize/ignore-default (function &optional (default-values (list nil)))
  "Make the given function memoizing on all parameters, which are
  compared using `equal'. However, do not memoize values which are
  equal to some member of `default-values', by default `nil'."
  (let ((memo (make-memo-container/hash-table)))
    (lambda (&rest args)
      ;; special constants allow to get at the container, and also
      ;; allow clearing everything.
      (cond
        ((eq (first args) +memo-clear+) (memo-clear/hash-table memo))
        ((eq (first args) +memo-container+) memo)
        (t
         (mvbind (result found-p) #1=(gethash args memo)
                 (if found-p
                     result
                     (aprog1 (apply function args)
                       (unless (member it default-values :test 'equal)
                         (setf #1# it))))))))))

