(defsystem ol-data-utils
  :serial t
  :depends-on ("ol-utils"
               "cl-prevalence"
               "local-time")
  :components ((:file "prevalence")
               (:file "dates")))
