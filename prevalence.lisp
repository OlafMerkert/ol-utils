(defpackage #:prevalence-utils
  (:use #:cl #:ol)
  (:export
   :define-prevalence-storage
   :storage
   :prevailing
   :prevalence-id))

(in-package :prevalence-utils)

(defmacro define-prevalence-storage
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

       (defmacro ,(symb 'define- storage) (var-name &optional (autoload t))
         `(progn
            (defvar ,var-name nil)
            (defun ,(symb 'load- var-name) ()
              (,',#2#)
              (setf ,var-name
                    (cl-prevalence:get-root-object ,',storage ',var-name)))
            (defun ,(symb 'save- var-name) ()
              (,',#2#)
              (setf (cl-prevalence:get-root-object ,',storage ',var-name)
                    ,var-name)
              (cl-prevalence:snapshot ,',storage)
              ,var-name)
            ,(when autoload
                   `(,(symb 'load- var-name))))))))

(defclass prevailing ()
  ((id :accessor prevalence-id))
  (:documentation "A base class for anything that needs to be persisted."))

