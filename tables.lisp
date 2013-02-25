(in-package #:ol-utils)

(export '(gethash/c
          sort-hash-table))

(defun gethash/c% (key table default-generator)
  "Get a value from a hash TABLE.  If it is not yet present, funcall
DEFAULT-GENERATOR and set the value to the result.  Furthermore,
return the result."
  (multiple-value-bind (value present-p)
      (gethash key table)
    (if present-p
        value
        (setf (gethash key table)
              (funcall default-generator)))))

(defmacro gethash/c (key table default)
  "Get a value from a hash table.  If it is not yet present, set it to
default and return the new value."
  `(gethash/c% ,key ,table (lambda () ,default)))

(defun sort-hash-table (hash-table predicate &key (key #'identity) entries)
  "Give a sorted list of the keys (or entries) of hash-table. The key
and predicate apply directly on the keys (or entries)."
  (let ((entries
         (iter (for (k e) in-hashtable hash-table )
               (if entries
                   (collect e)
                   (collect k)))))
    (sort entries predicate :key key)))
