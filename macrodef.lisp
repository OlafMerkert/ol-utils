(in-package #:ol-utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-symbol-start (start len s)
    "Check whether the symbol s matches the string start in the first
len characters."
     (and (symbolp s)
          (>= (length (symbol-name s))
              len)
          (string-equal (symbol-name s)
                        start
                        :start1 0
                        :end1 len)
          (subseq (symbol-name s) len))))

(defmacro def-symbol-p (start)
  "Define a predicate `start-symbol-p` on symbols, which classifies
symbols starting with start."
  `(defun ,(symb start '-symbol-p) (s)
     (test-symbol-start ,(symbol-name start)
                        ,(length (symbol-name start))
                        s)))

(defmacro def-symbol-transform (start1 start2)
  "Define a function that transforms symbols having predicate
`start1-symbol-p` ot symbols having predicate `start2-symbol-p`."
  `(defun ,(symb start1 '-symbol-to- start2 '-symbol) (s)
     (symb ,(symbol-name start2)
           (subseq (symbol-name s)
                   ,(length (symbol-name start1))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-symbol-p g!)
  (def-symbol-p o!)

  (def-symbol-transform o! g!))

;;; Macros with gensyms and once-only evaluation
(defmacro with-gensyms! (&body body)
  "Bind all symbols starting with g! to gensyms."
  (let ((syms (collect-symbols-if #'g!-symbol-p body t)))
    `(let ,(mapcar
            (lambda (s)
              `(,s (gensym ,(subseq (symbol-name s) 2))))
            syms)
      ,@body)))

(defmacro defmacro/g! (name args &body body)
  "Define a macro, where all symbols starting with g! will
automatically evaluate to gensyms."
  `(defmacro ,name ,args
     (with-gensyms! ,@body)))

(defmacro defmacro/o! (name args &body body)
  "Define a macro, where all symbols starting with g! will
automatically evaluate to gensyms.  Furthermore, args starting with o!
will be evaluated once and bound to the according symbol starting with
g!."
  (let* ((os (collect-symbols-if #'o!-symbol-p args))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro ,name ,args
       (with-gensyms!
         `(let ,(mapcar #'list (list ,@gs) (list ,@os))
            ,(progn ,@body))))))

(defmacro defmacro! (name args &body body)
  "Define a macro, where all symbols starting with g! will
automatically evaluate to gensyms.  Furthermore, args starting with o!
will be evaluated once and bound to the according symbol starting with
g!."
  (if (collect-symbols-if #'o!-symbol-p args)
      `(defmacro/o! ,name ,args ,@body)
      `(defmacro/g! ,name ,args ,@body)))

(ew
(def-symbol-p &)

(defun args->names (args)
  "Extract the parameter names from an argument list."
  (mapcan (lambda (x)
            (cond ((&-symbol-p x) nil)
                  ((listp x) (list (car x)))
                  (t (list x))))
          args)))

(defmacro! defalias (alias whatfor &optional args)
  "Create an alias for a function or macro."
  (unless args (setf args `(&rest ,g!rest)))
  `(defmacro ,alias (&whole ,g!args ,@args)
     ,(format nil "An alias for ~(`~A'~)." whatfor)
     (declare (ignore ,@(args->names args)))
     `(,',whatfor ,@(rest ,g!args))))

;; duality of syntax with defvar, and brevity
;; (defalias defpar defparameter (var val &optional doc))
(defmacro! defpar (var val &rest doc-or-others)
  "An alias for `defparameter', which also allows simultaneous
definition as in `setf'."
  (if (< (length doc-or-others) 2)
      `(defparameter ,var ,val ,@doc-or-others)
      `(progn
         (defparameter ,var ,val)
         ,@(mapcar #`(defparameter ,@a1) (partition doc-or-others 2)))))

(defmacro! defmacros! (name args &body body)
  "Produces a macro that \"applies\" defmacro! on each of its
arguments (first calls `mklist' on every argument)."
  `(defmacro! ,name (&rest ,g!configs)
     `(progn
        ,@(mapcar (lambda (,g!args)
                    (dbind ,args (mklist ,g!args)
                      ,@body))
                  ,g!configs))))
