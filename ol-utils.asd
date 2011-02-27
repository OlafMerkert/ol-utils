(defsystem ol-utils
  :serial t
  :depends-on ()
  :components ((:file "packages")
               (:file "basics")
               (:file "reader")
               (:file "macrodef")
               (:file "anaphoric")
               (:file "lists")
               (:file "functions")
               (:file "arrays")
               (:file "arithmetic")
               (:file "tables")
               (:file "clos")))
