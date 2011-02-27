(in-package #:ol-utils)

;;;; very basic Functions that are needed for macro writing etc

(export '(group
          flatten
          mkstr
          symb keyw
          defsymconstant))

;; Lists
(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))


(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

;; Symbols & Strings
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun keyw (&rest args)
  (values (intern (apply #'mkstr args)
                  :keyword)))

;; Defining persistent symbol constants
(defmacro defsymconstant (name)
  `(defconstant ,name (if (boundp ',name)
                          ,name
                          (gensym ,(symbol-name name)))))
