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

;; insert debug declaration with #d
(set-dispatch-macro-character
 #\# #\d
 (lambda (stream sub-char numarg)
   (declare (ignore stream sub-char))
   (setq numarg (or numarg 2))
   (unless (<= numarg 3)
     (error "Bad value for #d: ~A" numarg))
   `(declare (optimize (debug ,numarg)))))

;; use #` syntax to build lambda's that build lists
(defun |#`-reader| (stream sub-char numarg)
  "use #` syntax to build lambda's that build lists.  Parameters are
called a1, a2, ...j"
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  (let ((args (iter (for i from 1 to numarg)
                    (collect (symb 'a i)))))
   `(lambda ,args
      (declare (ignorable ,@args))
      ,(funcall
        (get-macro-character #\`) stream nil))))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)

;; declare variables as fixnum using #i (or integer with bounded bitlength)
(set-dispatch-macro-character
 #\# #\i
 (lambda (stream sub-char numarg)
   (declare (ignore sub-char))
   (let ((variable-names (read stream t nil t)))
     `(declare (,(if numarg
                     (let ((bound (expt 2 numarg)))
                      `(integer ,(- bound) ,bound))
                     'fixnum)
                ,@(mklist variable-names))))))
