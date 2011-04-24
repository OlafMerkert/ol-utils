(defsystem qt-utils
  :serial t
  :depends-on ("ol-utils" "qt")
  :components ((:module "qt"
                        :components ((:file "packages")
                                     (:file "camelcase")))))
