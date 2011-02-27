(in-package #:ol-utils)

(export '(gethash/c
          sort-hash-table))

(defmacro! gethash/c (o!key o!hash-table default)
  `(multiple-value-bind (,g!value ,g!present-p)
       (gethash ,g!key ,g!hash-table)
     (if ,g!present-p
         ,g!value
         (setf (gethash ,g!key ,g!hash-table)
               ,default))))

(defun sort-hash-table (hash-table predicate &key key)
  (let ((entries
         (loop for e being the hash-values of hash-table
            collect e)))
    (sort entries predicate :key key)))
