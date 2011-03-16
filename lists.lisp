(in-package #:ol-utils)

(export '(mklist
          range mrange lrange
          alternate
          reverse/n
          drop
          split
          singleton-p
          nconc1
          group-by))

(defun mklist (x)
  "Ensure that x is a list."
  (if (listp x) x (list x)))

(defun range (start &optional end (step 1))
  "Create a list with numbers between start and end with step.  start
is inclusive, end is exclusive."
  (unless end
    (setf end start
          start 0))
  (unless (plusp (* step (- end start)))
    (error "Invalid range from ~A to ~A by ~A" start end step))
  (do ((i start (+ step i))
       (l))
      ((>= i end) (nreverse l))
    (push i l)))

(defun mrange (start &optional end (step 1))
  "As range, only end is inclusive."
  (unless end
    (setf end start
          start 1))
  (unless (plusp (* step (- end start)))
    (error "Invalid mrange from ~A to ~A by ~A" start end step))
  (do ((i start (+ step i))
       (l))
      ((> i end) (nreverse l))
    (push i l)))

(defun lrange (seq)
  "Indizes of elements of seq."
  (range (length seq)))

(defun alternate (&rest lists)
  "Splice lists together by alternating through their elements,
i.e. the result will first have all the first elements of the lists,
then the second elements and so on.  The first arg can be a keyword
argument, `:reverse` which will reverse the order of the lists."
  (case (car lists)
    ((:normal)
     (apply #'mapcan #'list (cdr lists)))
    ((:reverse)
     (apply #'mapcan #'list (reverse (cdr lists))))
    (t (apply #'mapcan #'list lists))))

(defun reverse/n (n list)
  "Reverse only the first n entries of the list."
  (let ((a (subseq list 0 n))
        (b (subseq list n)))
    (nconc (nreverse a) b)))

(defun drop (n list)
  "Remove the n-th element from list."
  (nconc (subseq list 0 n)
         (subseq list (1+ n))))

(defun split (sep list)
  "Split the list at every occurence of sep.  The result will be a
list of the parts."
  (labels ((rec (remaining parts)
             (aif (position sep remaining)
                  (rec (nthcdr (1+ it) remaining)
                       (cons (subseq remaining 0 it) parts))
                  (cons (subseq remaining 0) parts))))
    (nreverse (rec list nil))))

(defun singleton-p (list)
  "Test whether list has exactly one element."
  (and (consp list)
       (null (cdr list))))

(defun nconc1 (list obj)
  "Destructively append obj to list."
  (nconc list (list obj)))

(defun group-by (key list &key (test #'equal))
  "Partition list into sublists according values of the function key.
Each sublist starts with the value of key on all of its elements (so
this is actually an alist).

You could think of this as calculating the inverse image of key on
list."
  (let (grouping)
    (dolist (l list)
      (let ((k (funcall key l)))
        (aif (assoc k grouping :test test)
             (nconc1 it l)
             (push (list k l) grouping))))
    grouping))
