(in-package #:ol-utils)

(export '(aif acond it))

;;; Commonly used anaphoric macros

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro/g! acond (&rest clauses)
  (when clauses
    `(let ((,g!test ,(caar clauses)))
       (if ,g!test
           (let ((it ,g!test))
             (declare (ignorable it))
             ,@(cdar clauses))
           (acond ,@(cdr clauses))))))
  