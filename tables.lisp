(in-package #:ol-utils)
(olr)

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
  (sort (if entries
            (table-values hash-table)
            (table-keys hash-table))
        predicate :key key))

(defun table-clean-if (test hash-table)
  "Remove all keys from `hash-table' where the value satisfies
`test'"
  (let (clean)
    (maphash (lambda (k v) (if (funcall test v) (push k clean))) hash-table)
    (dolist (c clean)
      (remhash c hash-table))
    (values hash-table (length clean))))

(defun sethash (key hash-table value &optional test)
  "Set `key' to `value' in `hash-table', unless `value' satisfies
`test'. In that case, remove the key from the `hash-table'."
  (if (and test (funcall test value))
      (remhash key hash-table)
      (setf (gethash key hash-table) value)))

(defun table-keys (hash-table)
  "Return a list of all keys in `hash-table'."
  (nprog1 list
    (maphash (ilambda (k v) (push k list)) hash-table)))

(defun table-values (hash-table)
  "Return a list of all values in `hash-table'."
  (nprog1 list
    (maphash (ilambda (k v) (push v list)) hash-table)))

(defun table->alist (hash-table)
  "Convert `hash-table' into an alist."
  (nprog1 list
    (maphash (ilambda (k v) (push (cons k v) list)) hash-table)))

(defun maphash-keys (function hash-table)
  "Map `function' just over the keys of `hash-table'."
  (maphash (lambda (k v) (declare (ignore v)) (funcall function k)) hash-table))

(defun maphash-values (function hash-table)
  "Map `function' just over the values of `hash-table'."
  (maphash (lambda (k v) (declare (ignore k)) (funcall function v)) hash-table))
