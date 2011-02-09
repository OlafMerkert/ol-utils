(in-package #:ol-utils)

(export '(test-symbol-start
          def-symbol-p
          def-symbol-transform
          defmacro/g!
          defmacro!))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun test-symbol-start (start len s)
     (and (symbolp s)
          (> (length (symbol-name s))
             len)
          (string-equal (symbol-name s)
                        start
                        :start1 0
                        :end1 len)
          (subseq (symbol-name s) len))))

(defmacro def-symbol-p (start)
  `(defun ,(symb start '-symbol-p) (s)
     (test-symbol-start ,(symbol-name start)
                        ,(length (symbol-name start))
                        s)))

(defmacro def-symbol-transform (start1 start2)
  `(defun ,(symb start1 '-symbol-to- start2 '-symbol) (s)
     (symb ,(symbol-name start2)
           (subseq (symbol-name s)
                   ,(length (symbol-name start1))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-symbol-p g!)
  (def-symbol-p o!)

  (def-symbol-transform o! g!))

;;; Macros with gensyms and once-only evaluation
(defmacro defmacro/g! (name args &body body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq (symbol-name s) 2))))
              syms)
         ,@body))))

(defmacro defmacro! (name args &body body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))
