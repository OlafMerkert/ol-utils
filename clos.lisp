(in-package #:ol-utils)

(export '(defclass/f
          undefmethoda udm))

(defmacro defclass/f (name direct-superclasses
                      direct-slots &rest options)
  "Enhancement of defclass: For slots where only a symbol is given for
a name, an accessor and initarg are automatically added."
  (let ((normalized-slots
         (mapcar (lambda (slot)
                   (etypecase slot
                     (symbol `(,slot :accessor ,slot :initarg ,(keyw slot)))
                     (list slot)))
                 direct-slots)))
    `(defclass ,name ,direct-superclasses
       ,normalized-slots ,@options)))
