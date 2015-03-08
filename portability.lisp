(in-package :ol-utils)
(olr)

(defun run-program (path-to-bin &rest args)
  #+sbcl (sb-ext:run-program path-to-bin args)
  #+ccl (ccl:run-program path-to-bin args)
  #-(or sbcl ccl) (error "Don't know how to run external programs."))

;;; make threading implementation dependend
(defun funcall+thread (function)
  #+sbcl (sb-thread:make-thread function)
  #+ccl (ccl:process-run-function "funcall+thread" function)
  #-(or sbcl ccl) (funcall function))
