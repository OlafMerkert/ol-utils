(in-package #:ol-utils)

(export '(aif acond it))

;;; Commonly used anaphoric macros

(defmacro aif (test then &optional else)
  "Anaphoric if.  The value of the test clause is bound to `it` in the
then clause.  (and the else clause, but there it will always be nil.)"
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro/g! acond (&rest clauses)
  "Anaphoric cond.  The value of the test clause is bound to `it`."
  (when clauses
    `(let ((,g!test ,(caar clauses)))
       (if ,g!test
           (let ((it ,g!test))
             (declare (ignorable it))
             ,@(cdar clauses))
           (acond ,@(cdr clauses))))))
