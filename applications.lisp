(in-package :ol-utils)

(defvar applications (make-hash-table))

(defun start-app (id)
  "Start a registered (and installed) cl application."
  (aif (gethash id applications)
       (funcall it)
       (format t "Unknown application ~A~%" id)))

(defmacro define-application (id system package function)
  "Define a starter function for ID that loads a SYSTEM, then calls
FUNCTION in PACKAGE. ID ought to be a keyword symbol, on the other
arguments we call mkstr, so use symbols or strings as you prefer."
  `(setf (gethash ,id applications)
         (lambda ()
           (ql:quickload ,(mkstr system))
           (funcall
            (intern ,(mkstr function)
                    ,(mkstr package))))))

(define-application :tvs tv-series-status tvs-clim tv-series-display)
