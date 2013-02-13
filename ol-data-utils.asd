(defsystem ol-data-utils
  :serial t
  :depends-on ("ol-utils"
               "cl-prevalence"
               "local-time"
               "split-sequence")
  :components ((:file "prevalence")
               (:file "dates")))
