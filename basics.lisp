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

;; SBCL workaround (from [[http://christophe.rhodes.io/notes/blog/posts/2014/naive_vs_proper_code-walking/]])
#+sbcl
(eval-when (:compile-toplevel :execute)
  (defun comma-implementation ()
    (typecase '`,x
      (symbol 'old)
      ((cons symbol (cons structure-object)) 'new)))
  (if (eql (comma-implementation) 'new)
      (pushnew 'cons-walkable-backquote *features*)
      (setq *features* (remove 'cons-walkable-backquote *features*))))

(defun flatten (x)
  "Flatten the tree structure of x to a list."
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   #+ol-utils::cons-walkable-backquote
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun flatten1 (x &optional (n 1))
  "Flatten the tree structure of x by one."
  (labels ((rec (x acc level)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   #+ol-utils::cons-walkable-backquote
                   ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc level))
                   ((<= n level) (cons (car x) (rec (cdr x) acc level)))
                   (t (rec (car x) (rec (cdr x) acc level) (+ level 1))))))
    (rec x nil 0)))

(defun collect-symbols-if (test form &optional unique)
  "Produce a list of all symbols satisfying `test' in the lisp `form'.
Also ensure no duplicates are in the result if `unique' is t."
  (let ((symbols (remove-if-not test (flatten form))))
    (if unique
        (remove-duplicates symbols)
        symbols)))

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
  `(unless (constantp ',name)
     (defconstant ,name ',(gensym (mkstr name '-constant))
       ,@(if documentation (list documentation)))))

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
              (pos (search "lambda" name :test #'char-equal)))
         (if pos (subseq name 0 pos)))))

;; some debugging and profiling helpers
(defparameter *debug-stream* t)
(defun dbug (string &rest args)
  "Output a debugging info to the standard output.  This mimics
format, but prepends 'DEBUG' and appends a line-break."
  (princ "DEBUG: " *debug-stream*)
  (apply #'format *debug-stream* string args)
  (terpri *debug-stream*))

(defparameter *progress-stream* t)
(declaim (inline progress-event))
(defun progress-event ()
  "Monitor loop iteration progress by printing dots to
`*progress-stream*', which defaults to `*standard-output*'."
  (princ "." *progress-stream*))


(defmacro bind-multi (bindings &body body)
  "Macro to define groups of similar functions or methods.
Syntax: (bind-multi ((v1 b1 b2)
                     (v2 b3 b4))
           body)"
  (let ((vars (mapcar #'first bindings))
        (vals1 (mapcar #'rest bindings)))
    (labels ((produce (vals)
               (unless (some #'null vals)
                 (append
                  (sublis (mapcar (lambda (var val) (cons var (first val))) vars vals)
                          (copy-tree body))
                  (produce (mapcar #'rest vals))))))
     `(progn
        ,@(produce vals1)))))
