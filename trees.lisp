(in-package #:ol-utils)

(export '(matching-tree-nodes
          map-tree
          map-tree-if))

(defun matching-tree-nodes (pred tree)
  "Give a list of all nodes (and leafs) that satisfy the predicate."
  (if (consp tree)
      (nconc (matching-tree-nodes pred (car tree))
             #1=(when (funcall pred tree)
                  (list tree))
             (matching-tree-nodes pred (cdr tree)))
      #1#))

(defun map-tree (fn tree)
  "Map FN on all leafs of TREE, conserving the structure."
  (if (atom tree)
      (funcall fn tree)
      (cons (map-tree fn (car tree))
            (map-tree fn (cdr tree)))))

(defun map-tree-if (pred fn tree)
  "Map FN just on the leafs of TREE, that satisfy PRED."
  (map-tree (lambda (leaf)
              (if (funcall pred leaf)
                  (funcall fn leaf)
                  leaf))
            tree))

(defun tree-find-if (pred tree)
  (cond ((funcall pred tree) tree)
        ((consp tree)
         (or (tree-find-if pred (car tree))
             (tree-find-if pred (cdr tree))))
        (t nil)))
