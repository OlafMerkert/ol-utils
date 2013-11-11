(in-package #:ol-utils)

(export '(aif acond it
          alambda self
          aand
          aprog1))

;;; Commonly used anaphoric macros

(defmacro aif (test then &optional else)
  "Anaphoric if. The value of the `test' clause is bound to `it` in
the `then' clause. (and the `else' clause, but there it will always be
nil.)"
  `(let ((it ,test))
     (if it ,then ,else)))

(defmacro awhen (test &body body)
  "Anaphoric when. The value of the `test' clause is bound to `it' in
  the `body'."
  `(let ((it ,test))
     (when it
       ,@body)))

(defmacro! ncond (name &body clauses)
  "Named cond.  The value of the test clause is bound to NAME."
  (when clauses
    `(let ((,g!test ,(caar clauses)))
       (if ,g!test
           (let ((,name ,g!test))
             (declare (ignorable ,name))
             ,@(cdar clauses))
           (ncond ,name ,@(cdr clauses))))))

(defmacro acond (&rest clauses)
  "Anaphoric cond.  The value of the test clause is bound to `IT`."
  `(ncond it ,@clauses))

(defmacro alambda (args &body body)
  "Anaphoric lambda.  Can call itself using `(self ...)`."
  `(labels ((self (,@args) ,@body))
     #'self))

(defmacro aand (&rest forms)
  "Anaphoric and.  The result of the previous form can be referred to
with `it`."
  (if (null forms)
      'it
      `(aif ,(first forms)
            (aand ,@(rest  forms)))))

(defmacro aprog1 (form &body body)
  "as PROG1, but FORM is bound to IT in BODY."
  `(let ((it ,form))
     ,@body
     it))

