(defpackage #:prevalence-utils
  (:use #:cl #:ol)
  (:export
   :define-prevalence-storage
   :storage))

(in-package :prevalence-utils)

(defmacro! define-prevalence-storage
    (directory &optional (storage 'storage))
  (let ((dir-var (symb storage '-directory)))
   `(progn
      (defparameter ,dir-var ,directory)
      (defvar ,storage nil)

      (defun ,#2=(symb 'init- storage) ()
          (unless ,storage
            (ensure-directories-exist ,dir-var)
            (setf ,storage
                  (cl-prevalence:make-prevalence-system ,dir-var))))

      (defmacro ,(symb 'define- storage) (,g!var-name)
        `(progn
           (defvar ,,g!var-name nil)
           (defun ,(symb 'load- ,g!var-name) ()
             (,',#2#)
             (setf ,,g!var-name
                   (cl-prevalence:get-root-object ,',storage ',,g!var-name)))
           (defun ,(symb 'save- ,g!var-name) ()
             (,',#2#)
             (setf (cl-prevalence:get-root-object ,',storage ',,g!var-name)
                   ,,g!var-name)
             (cl-prevalence:snapshot ,',storage)
             ,,g!var-name)
           (,(symb 'load- ,g!var-name)))))))
