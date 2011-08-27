(in-package #:qt-utils)

(export '(defqclass))

(defgeneric translate-options (key options))

(defmethod translate-options (key options)
  options)

(defmethod translate-options ((key (eql :signals)) options)
  `(:signals ,@(mapcar (lambda (s)
                         (list (qsignature s)))
                       (rest options))))

(defmethod translate-options ((key (eql :slots)) options)
  `(:slots ,@(mapcar (lambda (s)
                       ;; either the second part is a lambda form or
                       ;; we use a method for this
                       (if (and (consp s) (lambda-form-p (second s)))
                           `(,(qsignature (first s)) ,(second s))
                           `(,(qsignature s) ,s)))
                     (rest options))))

(defmethod translate-options ((key (eql :override)) options)
  `(:override ,@(mapcar (lambda (s)
                          `(,(qfunname s) ,s))
                        (rest options))))

(defmacro defqclass (name direct-superclasses direct-slots &rest options)
  "Here Qt superclasses ought to be marked with a starting q-.  There
should only be one."
  (let ((lisp-superclasses (remove-if #'q-symbol-p direct-superclasses))
        (qt-superclasses   (mapcar #'qclassname
                                   (remove-if-not #'q-symbol-p direct-superclasses))))
    (assert (length=1 qt-superclasses))
    `(defclass ,name ,lisp-superclasses
       (,@direct-slots)
       (:metaclass qt:qt-class)
       (:qt-superclass ,@qt-superclasses)
       ,@(mapcar (lambda (option) (translate-options (car option) option)) options))
    ;; do not Supply a default init method!
    ))
