(in-package #:qt-utils)

(export '(lisp->camel
          q-symbol-p
          qfunname
          qclassname
          qsignature))

(defun lisp->camel (string &optional start-big)
  "transform LISP-style to lispStyle (or LispStyle)."
  (let ((camel 
         (remove #\- (string-capitalize string))))
    (if start-big camel
        (string-downcase camel :end 1))))

(defun qfunname (symbol)
  "generate Qt style function name from lisp-style function name.
Example: set-parent becomes setParent"
  (lisp->camel (mkstr symbol)))

(defun q-symbol-p (symbol)
  (test-symbol-start "Q-" 2 symbol))

(defun qclassname (symbol)
  "generate Qt class name from lisp-style class name.  Example:
box-layout becomes QBoxLayout"
  (lisp->camel (if (q-symbol-p symbol) symbol
                   (mkstr "Q-" symbol)) t))

(defun qsignature (signal-or-slot-sexp)
  "Generate the signature of a Qt signal/slot.  Currently no
parameters are considered."
  (format nil "~A()"
          (qfunname (first (mklist signal-or-slot-sexp)))))
