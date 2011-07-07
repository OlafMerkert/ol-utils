(in-package #:qt-utils)

(export '(lisp->camel
          instance
          qnew
          c
          qconnect
          qlayout
          make-qapp))

(defun lisp->camel (string &optional start-big)
  "transform LISP-style to lispStyle (or LispStyle)."
  (let ((camel 
         (remove #\- (string-capitalize string))))
    (if start-big camel
        (string-downcase camel :end 1))))

(defmacro/g! qnew (classname &rest args)
  (let ((constructor-args (remove-if-not #'atom args))
        (set-properties   (remove-if-not #'consp args)))
    `(let ((,g!instance (optimized-new ,(lisp->camel (mkstr :q- classname) t)
                                       ,@constructor-args)))
       ,@(mapcar #`(optimized-call t ,g!instance
                                   ,(lisp->camel (mkstr (first a1)))
                                   ,@(rest a1))
                 set-properties)
       ,g!instance)))

(defmacro c (function instance &rest args)
  `(optimized-call t ,instance
                   ,(lisp->camel (mkstr function))
                   ,@args))

(defmacro qconnect (source signal sink slot)
  `(optimized-call T "QObject" "connect"
                   ,source (QSIGNAL ,signal)
                   ,sink (QSLOT ,slot)))

(defmacro qlayout (type &body widgets)
  `(qnew ,(mkstr type :-layout)
         ,@(mapcar #`(:add-widget ,@(mklist a1))
                  widgets)))

(defun make-qapp ()
  (setf qt-user:*application*
        (make-qapplication)))

(defgeneric translate-options (key options))

(defmethod translate-options (key options)
  (cons key options))

(defmethod translate-options ((key (eql :override)) method-names)
  `(,key ,(mapcar #`(,(lisp->camel (symbol-name a1)) ,a1) method-names)))

(defmacro defqtclass (name direct-superclasses direct-slots &rest options)
  (let ((lisp-superclasses (remove-if-not #'symbolp direct-superclasses))
        (qt-superclasses   (remove-if-not #'stringp direct-superclasses)))
    `(defclass ,name ,lisp-superclasses
       (,@direct-slots)
       (:metaclass qt:qt-class)
       (:qt-superclass ,@qt-superclasses)
       ,@(mapcar (lambda (o) (translate-options (car o) (cdr o))) options))))
