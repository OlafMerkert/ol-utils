(defsystem ol-data-utils
  :serial t
  :depends-on (ol-utils
               cl-prevalence
               local-time
               split-sequence
               cl-ppcre)
  :components ((:file "prevalence")
               (:file "dates")))
