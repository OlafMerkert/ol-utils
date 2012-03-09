(defsystem ol-data-utils
  :serial t
  :depends-on ("ol-utils"
               "cl-prevalence")
  :components ((:file "prevalence")))
