(in-package #:ol-utils)

(export '(defclass/f
          create-standard-print-object))

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

(defmacro! create-standard-print-object
    (class &rest slots)
  "For the given CLASS, define a print-object method which output
#<CLASS SLOTS> where SLOTS is a tree of slot identifiers and strings
and is printed with the values of the slots put in."
  (let ((slots-flat (flatten slots)))
    `(defmethod print-object ((,g!object ,class) ,g!stream)
       (print-unreadable-object (,g!object ,g!stream :type t)
         (with-slots ,(remove-if-not #'symbolp slots-flat) ,g!object
           (format ,g!stream
                   ,(format nil "~{~A~^ ~}"
                            (mapcar (alambda (x)
                                        (if (listp x)
                                            (format nil "[~{~A~^ ~}]"
                                                    (mapcar #'self x))
                                            "~A"))
                                    slots))
                   ,@slots-flat)))
       ;; finally return object
       ,g!object)))

(defun class-name-of (object)
  (class-name (class-of object)))
