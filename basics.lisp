(in-package #:ol-utils)

;;;; very basic Functions that are needed for macro writing etc

(export '(group
          flatten
          flatten1
          mkstr
          symb keyw
          defconstant/g
          defsymconstant
          ew
          lambda-form-p
          bind-multi
          dbug))

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

(defun flatten1 (x &optional (n 1))
  "Flatten the tree structure of x by one."
  (labels ((rec (x acc level)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   ((<= n level) (cons (car x) (rec (cdr x) acc level)))
                   (t (rec (car x) (rec (cdr x) acc level) (+ level 1))))))
    (rec x nil 0)))

;; Symbols & Strings
(defun mkstr (&rest args)
  "Concatenate the string values of all args."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  "Build a symbol by concatenating all args (which may be symbols or
strings or whatever."
  (values (intern (apply #'mkstr args))))

(defun symb+ (package &rest args)
  "Like symb, but put the symbol in the specified package."
  (values (intern (apply #'mkstr args) package)))

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
  "Define a constant with a symbol as value."
  `(defconstant ,name ',(symb 'ol-sym-constant- name)
     ,@(if documentation (list documentation))))
;; TODO figure out what to do about collisions

;; Abbreviate eval-when
(defmacro ew (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))

;; detecting lambda forms
(defun lambda-form-p (s-exp)
  "Test whether this is a cons, where the car is symbol whose name
  contains lambda.  It returns the part before the lambda (as a string) if so."
  (and (consp s-exp)
       (symbolp (car s-exp))
       (let* ((name (symbol-name (car s-exp)))
              (pos (search "lambda" name 
                          :test #'char-equal)))
         (if pos (subseq name 0 pos)))))

;;
(defmacro bind-multi (bindings &body body)
  "Macro to define groups of similar functions or methods.
Syntax: (bind-multi ((v1 b1 b2)
                     (v2 b3 b4))
           body)"
  (let ((vars (mapcar #'first bindings))
        (vals (transpose-list (mapcar #'rest bindings))))
   `(progn
      ,@(mapcan (lambda (vals)
                  (sublis (mapcar #'cons vars vals) (copy-tree body)))
                vals))))

;;
(defun dbug (string &rest args)
  "Output a debugging info to the standard output.  This mimics
format, but prepends 'DEBUG' and appends a line-break."
  (princ "DEBUG: ")
  (apply #'format t string args)
  (terpri))

(defmacro! pass-symbol ((op o!arg))
  "Only apply a univariate function `op', if the actual `arg' is not a
symbol. For symbols, just pass them."
  `(if (symbolp ,g!arg)
       ,g!arg
       (,op ,g!arg)))
