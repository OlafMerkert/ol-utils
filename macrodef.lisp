(in-package #:ol-utils)

(export '(test-symbol-start
          def-symbol-p
          def-symbol-transform
          with-gensyms!
          defmacro/g!
          defmacro!))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-symbol-start (start len s)
    "Check whether the symbol s matches the string start in the first
len characters."
     (and (symbolp s)
          (> (length (symbol-name s))
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
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
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
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
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
  (if (member-if #'o!-symbol-p (flatten args))
      `(defmacro/o! ,name ,args ,@body)
      `(defmacro/g! ,name ,args ,@body)))

(defmacro! defalias (alias whatfor &optional args)
  "Create an alias for a function or macro."
  `(defmacro ,alias (&whole ,g!args ,@args)
     (declare (ignorable ,@args)) ; TODO filter out &stuff
     `(,',whatfor ,@,g!args)))
