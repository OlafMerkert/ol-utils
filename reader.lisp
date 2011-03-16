(in-package #:ol-utils)

;;;; some very useful reader macros

;; insert speed/safety declarations with #f
(set-dispatch-macro-character
 #\# #\f
 (lambda (stream sub-char numarg)
   (declare (ignore stream sub-char))
   (setq numarg (or numarg 3))
   (unless (<= numarg 3)
     (error "Bad value for #f: ~A" numarg))
   `(declare (optimize (speed ,numarg)
                       (safety ,(- 3 numarg))))))

;; use #` syntax to build lambda's that build lists
(defun |#`-reader| (stream sub-char numarg)
  "use #` syntax to build lambda's that build lists.  Parameters are
called a1, a2, ...j"
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
               collect (symb 'a i))
     ,(funcall
       (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)
