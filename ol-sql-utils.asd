(defsystem ol-sql-utils
  :depends-on (ol-utils
               ol-data-utils
               clsql
               clsql-sqlite3)
  :serial t
  :components ((:file "clsql-initialisation-helpers")
               (:file "clsql-query-helpers")))
