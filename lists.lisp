(in-package #:ol-utils)

(export '(mklist
          range mrange lrange
          alternate
          reverse/n
          drop
          split
          singleton-p
          append1 nconc1
          group-by
          collect
          mappend
          assoc1
          filter))

(defun mklist (x)
  "Ensure that x is a list."
  (if (listp x) x (list x)))


(defmacro! do-range ((var
                      o!start &optional o!end (o!step 1)
                      result-form
                      (default-start 0) end-inclusive)
                     local-binds
                     &body body)
  `(progn
     ;; when only one argument is given, it is the end
     (unless ,g!end
       (setf ,g!end ,g!start
             ,g!start ,default-start))
     ;; check whether range is valid, i.e. step size is non-zero
     (when (zerop ,g!step)
       (error "Invalid range from ~A to ~A by ~A" ,g!start ,g!end ,g!step))
     ;; provide an anaphoric variable length which describes the
     ;; length of the resulting sequence.
     (multiple-value-bind (length ,g!rest)
         (ceiling (max 0 (/ (- ,g!end ,g!start) ,g!step)))
       (declare (ignorable length ,g!rest))
       ,(when end-inclusive
                `(if (zerop ,g!rest) (incf length)))
      ;; different comparators for negative step
      (if (plusp ,g!step)
          (do ((,var ,g!start (+ ,var ,g!step))
               ,@local-binds)
              ((,(if end-inclusive '> '>=)
                 ,var ,g!end) ,result-form)
            ,@body)
          (do ((,var ,g!start (+ ,var ,g!step))
               ,@local-binds)
              ((,(if end-inclusive '< '<=)
                 ,var ,g!end) ,result-form)
            ,@body)))))


(defun range (start &optional end (step 1))
  "Create a list with numbers between start and end with step.  start
is inclusive, end is exclusive."
  (do-range (i start end step (nreverse l))
      (l)
    (push i l)))


(defun mrange (start &optional end (step 1))
  "As range, only end is inclusive (Matlab-Style)."
  (do-range (i start end step (nreverse l) 1 t)
      (l)
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

(defun append1 (list obj)
  "Append obj to list."
  (append list (list obj)))

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

(defun collect (list &key singletons (test #'eql) (key #'identity))
  "Group subsequent elements of LIST if they satisfy TEST.  Unless SINGLETONS is T, groups of only one element are flattened again."
  (labels ((compact-group (g)
             (if (or (cdr g) singletons)
                 (nreverse g)
                 (car g)))
           (rec (list previous previous-group acc)
             (if (null list)
                 (cons (compact-group previous-group) acc)
                 (let ((k (funcall key (car list))))
                   (if (funcall test previous k)
                       (rec (cdr list) k (cons (car list) previous-group) acc)
                       (rec (cdr list) k (list (car list))
                            (cons (compact-group previous-group) acc)))))))
    (if (null list) nil
        (nreverse (rec (cdr list) (funcall key (car list))
                       (list (car list)) nil)))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun assoc1 (key alist &optional default &rest params)
  "Retrieve value assigned to KEY from ALIST.  If not found, return
DEFAULT.  A second value indicates whether KEY was found (like
gethash)."
  (aif (apply #' assoc key alist params)
       (values (cdr it) t)
       (values default nil)))

(defun filter (fn lst &optional acc)
  "filter lst through fn, dropping any nil values."
  (if lst
      (aif (funcall fn (car lst))
           (filter fn (cdr lst) (cons it acc))
           (filter fn (cdr lst) acc))
      (nreverse acc)))
