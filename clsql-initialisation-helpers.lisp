(defpackage :clsql-helpers
  (:use :cl :ol :clsql)
  (:export
   #:ensure-sequences
   #:ensure-tables
   #:ensure-tables-with-indices
   #:define-sqlite3-database))

(in-package :clsql-helpers)

;;; some general purpose setup utilities for databases
(defun ensure-sequences (sequences)
  "Make sure the sequence identified by the given list of symbols
actually exist in the database. Return the symbols for which we
created sequences."
  (filter (lambda (seq)
            (unless (sequence-exists-p seq)
              (create-sequence seq)
              seq))
          sequences))

(defun ensure-tables (tables)
  "Create the tables identified by the given list of symbols if they
do not exist yet, and return the symbols of created tables."
  (filter (lambda (table)
            (unless (table-exists-p table)
              (create-view-from-class table)
              table))
          tables))

(defun ensure-tables-with-indices (tables-and-indices)
  "Extend `create-tables': if an element of `tables-and-indices' is a
`cons', treat the `car' as table names and make sure every column
mentioned in `cdr' has the appropriate (single) column index."
  (let (created)
    (mapc (lambda (table-and-indices)
            (dbind (table . indices) (mklist table-and-indices)
              (unless (table-exists-p table)
                (create-view-from-class table)
                (push table created))
              (dolist (index indices)
                (let ((index-id (symb table '- index '-index)))
                  (unless (index-exists-p index-id)
                    (create-index index-id :on table :attributes (list index)
                                  :unique nil)
                    (push index-id created))))))
          tables-and-indices)
    (nreverse created)))

(defun connect-sqlite3-db (path)
  (ensure-directories-exist path)
  (let ((db (connect (list path) :database-type :sqlite3)))
    (execute-command "PRAGMA synchronous=OFF" :database db)
    (setf *default-caching* nil)
    db))

(defmacro define-sqlite3-database (name path &key sequences tables)
  (let ((db-name (symb '* name '-db*)))
    `(progn
       (defvar ,db-name nil)

       (defun ,(symb 'setup- name) ()
         (append (ensure-sequences ,sequences)
                 (ensure-tables-with-indices ,tables)))

       (defun ,(symb 'connect- name) (&optional (path ,path))
         (unless ,db-name
           (setf ,db-name (connect-sqlite3-db path))
           (cons ',db-name (,(symb 'setup- name))))))))
