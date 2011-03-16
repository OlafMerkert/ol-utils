(in-package #:ol-utils)

(export '(matching-tree-nodes))

(defun matching-tree-nodes (pred tree)
  "Give a list of all nodes (and leafs) that satisfy the predicate."
  (if (consp tree)
      (nconc (matching-tree-nodes pred (car tree))
             #1=(when (funcall pred tree)
                  (list tree))
             (matching-tree-nodes pred (cdr tree)))
      #1#))
