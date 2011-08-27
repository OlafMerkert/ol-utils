(in-package #:qt-utils)

(export '(lisp->camel
          q-symbol-p
          qfunname
          qclassname
          qtype
          qsignature))

(defun lisp->camel (string &optional start-big)
  "transform LISP-style to lispStyle (or LispStyle)."
  (let ((camel 
         (remove #\- (string-capitalize string))))
    (if start-big camel
        (string-downcase camel :end 1))))

(defun same-symbol-name (s1 s2)
  "Teste, ob zwei Symbole die gleichen Symbolnamen haben (ignoriert
Paketzugehörigkeit)."
  (string-equal (symbol-name s1)
                (symbol-name s2)))

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

(defparameter *qt-special-types*
  '((integer . "int")
    (int . "int")
    (boolean . "bool")
    (bool . "bool")
    (void . "void")))

(defun qtype (symbol)
  "signatures require specification of types.  This function basically
  manages special treatment for elementary type like int, bool etc.
  For the other we just fall back on qclassname."
  (or (assoc1 symbol *qt-special-types* nil :test #'same-symbol-name)
      (qclassname symbol)))

(defun qsignature (signal-or-slot-sexp)
  "Generate the signature of a Qt signal/slot.  If the first element
is :return, consider the following one the return type."
  (let ((sos (mklist signal-or-slot-sexp)))
    (if (eql :return (first sos))
        (format nil "~A ~A(~{~A~^, ~})"
                (qtype (second sos))
                (qfunname (third sos))
                (mapcar #'qtype (nthcdr 3 sos)))
        (format nil "~A(~{~A~^, ~})"
                (qfunname (first sos))
                (mapcar #'qtype (rest sos))))))
