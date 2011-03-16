(in-package #:ol-utils)

(export '(gethash/c
          sort-hash-table))

(defmacro! gethash/c (o!key o!hash-table default)
  "Get a value from a hash table.  If it is not yet present, set it to
default and return the new value."
  `(multiple-value-bind (,g!value ,g!present-p)
       (gethash ,g!key ,g!hash-table)
     (if ,g!present-p
         ,g!value
         (setf (gethash ,g!key ,g!hash-table)
               ,default))))

(defun sort-hash-table (hash-table predicate &key (key #'identity))
  "Give a sorted list of the entries of hash-table.  The key and
predicate apply directly on the entries."
  (let ((entries
         (loop for e being the hash-values of hash-table
            collect e)))
    (sort entries predicate :key key)))
