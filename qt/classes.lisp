(in-package :qt-utils)

(export '())

(defgeneric translate-options (key options))

(defmethod translate-options (key options)
  (cons key options))

(defmethod translate-options ((key (eql :override)) method-names)
  `(,key ,(mapcar #`(,(lisp->camel (symbol-name a1)) ,a1) method-names)))

(defmacro defqclass (name direct-superclasses direct-slots &rest options)
  (let ((lisp-superclasses (remove-if-not #'symbolp direct-superclasses))
        (qt-superclasses   (remove-if-not #'stringp direct-superclasses)))
    `(defclass ,name ,lisp-superclasses
       (,@direct-slots)
       (:metaclass qt:qt-class)
       (:qt-superclass ,@qt-superclasses)
       ,@(mapcar (lambda (o) (translate-options (car o) (cdr o))) options))))
