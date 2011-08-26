(in-package #:ol-utils)

;;;; very basic Functions that are needed for macro writing etc

(export '(group
          flatten
          mkstr
          symb keyw
          defconstant/g
          defsymconstant
          ew))

;; Lists
(defun group (source n)
  "Partition the list source into a list of lists of length n."
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defun flatten (x)
  "Flatten the tree structure of x to a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; Symbols & Strings
(defun mkstr (&rest args)
  "Concatenate the string values of all args."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Build a symbol by concatenating all args (which may be symbols or
strings or whatever."
  (values (intern (apply #'mkstr args))))

(defun keyw (&rest args)
  "The same as symb, but for keyword symbols."
  (values (intern (apply #'mkstr args)
                  :keyword)))

;; Defining persistent symbol constants
(defmacro defconstant/g (name expr &optional documentation)
  "Define a constant using a non-constant expression."
  `(defconstant ,name (if (boundp ',name)
                          ,name
                          ,expr)
     ,documentation))

(defmacro defsymconstant (name &optional documentation)
  "Define a constant with a gensym as value."
  `(defconstant ,name (if (boundp ',name)
                          ,name
                          (gensym ,(symbol-name name)))
     ,documentation))

;; Abbreviate eval-when
(defmacro ew (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))