(in-package #:ol-utils)

(export '(mklist
          mkatom
          range mrange lrange
          alternate
          reverse/n
          drop
          split
          length=1
          append1 nconc1
          group-by
          collect
          compress
          mappend
          assoc1
          assoc1a
          filter
          splitn
          last1
          starts-with
          maximise minimise
          transpose-list
          make-queue enqueue dequeue))

(defun mklist (x)
  "Ensure that x is a list."
  (if (listp x) x (list x)))

(defun mkatom (x)
  "Ensure that X is an atom, by applying CAR until it is."
  (if (consp x)
      (mkatom (car x))
      x))

(defmacro-driver (FOR var BETWEEN start AND end &optional BY (step 1))
  (with-gensyms!
    (let ((kwd (if generate 'generate 'for)))
      `(progn
         (with ,g!start      = ,start)
         (with ,g!end        = ,end)
         (with ,g!increasing = (<= ,g!start ,g!end))
         (with ,g!step       = (if ,g!increasing (abs ,step) (- (abs ,step))))
         (,kwd ,var next
               (cond ((first-iteration-p) ,g!start)
                     ((if ,g!increasing
                          (> (incf ,var ,g!step) ,g!end)
                          (< (incf ,var ,g!step) ,g!end))
                      (terminate))
                     (t ,var)))))))

(defmacro-driver (FOR var BETWEEN start AND-WITHOUT end &optional BY (step 1))
  (with-gensyms!
    (let ((kwd (if generate 'generate 'for)))
      `(progn
         (with ,g!start      = ,start)
         (with ,g!end        = ,end)
         (with ,g!increasing = (<= ,g!start ,g!end))
         (with ,g!step       = (if ,g!increasing (abs ,step) (- (abs ,step))))
         (,kwd ,var next
               (cond ((first-iteration-p) ,g!start)
                     ((if ,g!increasing
                          (>= (incf ,var ,g!step) ,g!end)
                          (<= (incf ,var ,g!step) ,g!end))
                      (terminate))
                     (t ,var)))))))

(defmacro with-range-spec ((start end &optional (default-start 0)) &body body)
  "both start and end ought to be symbols!!"
  `(progn
     (unless ,end
      (setf ,end ,start
            ,start ,default-start))
     ,@body))

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

(defmacro! dotimes+ ((var
                      o!start &optional o!end
                      result-form
                      (default-start 0) end-inclusive)
                     local-binds
                     &body body)
  "a simpler version of do-range (or fancier version of dotimes) where
  always step = 1."
  `(progn
     (unless ,g!end
       (setf ,g!end ,g!start
             ,g!start ,default-start))
     (do ((,var ,g!start (+ ,var 1))
          ,@local-binds)
         ((,(if end-inclusive '> '>=)
            ,var ,g!end) ,result-form)
       ,@body)))

(defun range (start &optional end (step 1))
  "Create a list with numbers between start and end with step.  start
is inclusive, end is exclusive."
  (with-range-spec (start end 0)
    (iter (for i between start and-without end by step)
          (collect i))))

(defun mrange (start &optional end (step 1))
  "As range, only end is inclusive (Matlab-Style)."
  (with-range-spec (start end 1)
    (iter (for i between start and end by step)
          (collect i))))

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

(defun droplast (list &optional acc)
  "Remove the last element from list."
  (if (or (null list) (null (cdr list)))
      (nreverse acc)
      (droplast (cdr list) (cons (car list) acc))))


(defun split (sep list)
  "Split the list at every occurence of sep.  The result will be a
list of the parts."
  (labels ((rec (remaining parts)
             (aif (position sep remaining)
                  (rec (nthcdr (1+ it) remaining)
                       (cons (subseq remaining 0 it) parts))
                  (cons (subseq remaining 0) parts))))
    (nreverse (rec list nil))))

(defun split-last (list)
  "destructively split the last entry from the list. return (values
list last)"
  (if (cdr list)
      ;; first deal with lists of more than 1 element
      (let* ((l (last list 2))
             (e (second l)))
        (setf (cdr l) nil)
        (values list e))
      ;; then the special case of just one or none element.
      (values nil (first list))))

(defun length=0 (seq)
  "Test whether sequence `seq' has no element."
  (or (null seq)
      (zerop (length seq))))

(defun length=1 (seq)
  "Test whether sequence `seq' has exactly one element."
  (or (and (consp seq)
           (null (cdr seq)))
      (= (length seq) 1)))

(defun length=n (n)
  "Produce a function that tests if a sequence has precisely length
`n'."
  (lambda (seq) (= (length seq) n)))

(defun length>0 (seq)
  "Test whether sequence `seq' is non-empty."
  (> (length seq) 0))

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
    ;; put the grouping in order
    (nreverse grouping)))

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

;;; todo maybe move this
(defsymconstant +keep+)

(defun recons (cons car cdr)
  "Reuse a cons cell.  "
  (unless (eql car +keep+)
    (setf (car cons) car))
   (unless (eql cdr +keep+)
    (setf (cdr cons) cdr))
   cons)

(defun compress (list &key singletons (test #'eql) (key #'identity))
  "Count the numbers of subsequent elements of LIST if they satisfy
TEST as (FIRST-OCCURENCE . COUNT).  Unless SINGLETONS is T, counts of
1 are flattened again."
  (labels ((compact-group (g)
             (if (or singletons (< 1 (cdr g)))
                 g
                 (car g)))
           (rec (list previous previous-group acc)
             (if (null list)
                 (cons (compact-group previous-group) acc)
                 (let ((k (funcall key (car list))))
                   (if (funcall test previous k)
                       (rec (cdr list) k (recons previous-group
                                                 +keep+
                                                 (+ 1 (cdr previous-group)))
                            acc)
                       (rec (cdr list) k (cons (car list) 1)
                            (cons (compact-group previous-group) acc)))))))
    (if (null list) nil
        (nreverse (rec (cdr list) (funcall key (car list))
                       (cons (car list) 1) nil)))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun assoc1 (key alist &optional default &rest params)
  "Retrieve value assigned to KEY from ALIST.  If not found, return
DEFAULT.  A second value indicates whether KEY was found (like
gethash)."
  (aif (apply #'assoc key alist params)
       (values (cdr it) t)
       (values default nil)))

(defun assoc1* (&rest arguments)
  "Descend a tree of alists, given as the last argument.
Essentially (assoc1* x y z alist) = (assoc1 x (assoc1 y (assoc1 z
alist))), but more basic."
  (dbind (alist . keys) (reverse arguments)
    (labels ((rec (keys alist)
               (acond ((null keys)
                       (values alist t))
                      ((assoc (car keys) alist)
                       (rec (cdr keys) (cdr it)))
                      (t (values nil nil)))))
      (rec keys alist))))

(defun assoc1a (key alist &optional default &rest params)
  "Like assoc1, but take the CADR instead of the CDR of the result.  (Useful for list structures)."
  (aif (apply #'assoc key alist params)
       (values (cadr it) t)
       (values default nil)))

(defmacro! alist-bind (bindings o!alist &body body)
  "Selectively bind entries of an `alist' to variables. `bindings' is
a list of tuples (var key), where at the first occurence of `key' (the
`car') in `alist' we bind the corresponding value (the `cdr') to
`var'."
  (let ((vars (mapcar #'unbox1 bindings))
        (keys (mapcar (lambda (x) (if (consp x) (second x) (keyw x))) bindings))
        (var-set (list->gensyms :var-set bindings)))
    `(let ,vars
       ;; make sure that eveyr variable will be set only the first
       ;; time we see the corresponding key.
       (let ,var-set
         (dolist (,g!item ,g!alist)
           (case (car ,g!item)
             ,@(mapcar #3`((,a2) (unless ,a3
                                   (setf ,a1 (cdr ,g!item)
                                         ,a3 t)))
                       vars keys var-set))))
       ,@body)))

(defmacro! plist-bind (bindings o!plist &body body)
  "Bind keyword keys of `plist' to the corresponding variables.
`bindings' should just e a list of symbols."
  `(let ,(mapcar #`(,a1 (getf ,g!plist ,(keyw a1))) bindings)
     ,@body))

(defun all-p (list)
  "Test whether all elements of `list' are non-nil."
  (if (null list)
      t
      (if (car list)
          (all-p (cdr list))
          nil)))
(defun filter (fn lst &optional acc)
  "filter lst through fn, dropping any nil values."
  (if lst
      (aif (funcall fn (car lst))
           (filter fn (cdr lst) (cons it acc))
           (filter fn (cdr lst) acc))
      (nreverse acc)))

(defun filter* (fn list &rest lists)
  "filter `list' through `fn', dropping any nil values."
  (labels ((rec (lists acc)
             (if (member nil lists)
                 (nreverse acc)
                 (aif (apply fn (mapcar #'car lists))
                      (rec (mapcar #'cdr lists) (cons it acc))
                      (rec (mapcar #'cdr lists) acc)))))
    (rec (cons list lists) nil)))

(defun splitn (list &optional (n 2))
  "Split `list' into sequences of all `n'-th elements."
  (let ((splits (make-array n :initial-element nil)))
    (iter (for l in list)
          (for i initially 0 then (mod (+ i 1) n))
          (push l (aref splits i)))
    (map 'list #'nreverse splits)))

(defun splitn/values (list &optional (n 2))
  (values-list (splitn list n)))

(defun partition (list &optional (n 2))
  "Split `list' into sequences of `n' elements."
  (labels ((rec (list acc)
             (cond ((null list) (nreverse acc))
                   ((null (nthcdr (- n 1) list)) (nreverse (cons list acc)))
                   (t (rec (subseq list n) (cons (subseq list 0 n) acc))))))
    (rec list nil)))

(defun last1 (list)
  "Return the last element of LIST."
  (first (last list)))

;; TODO this can be improved
(defsetf last1 (list) (value)
  `(setf (first (last ,list)) ,value))

(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun maximise (seq &key (key #'identity))
  "Find the maximum value of the sequence."
  (apply #'max (map 'list key seq)))

(defun minimise (seq &key (key #'identity))
  "Find the minimum value of the sequence."
  (apply #'min (map 'list key seq)))

(defun transpose-list (list)
  "Create a transposition of the LIST of lists."
  (let ((max-len (maximise list :key #'length)))
    (mapcar (lambda (i)
              (mapcar (lambda (l) (nth i l)) list))
            (range max-len))))

;; simple queue datatype, taken from ANSI Common Lisp by Paul Graham
(defun make-queue ()
  "Create a simple queue.  Implemented as a cons (list . last), where
last points to the last cons cell of list and list starts with the
next object to be dequeued."
  (cons nil nil))

(defun enqueue (obj q)
  "Put an object into the queue."
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  "Get the next object from the queue (or just nil)."
  (pop (car q)))

(defun peek (q)
  "Peek at the next object in the queue."
  (car q))

(defun splice-in (item list)
  "Create a copy of LIST, but with ITEM inserted between consecutive
elements."
  (labels ((acc (acc list)
             (if list
                 (acc (list* (car list) item acc) (cdr list))
                 (nreverse acc))))
    (acc (list (first list)) (rest list))))

(defun popn% (n list)
  "Cut off the first N elements of list, and return this subsequence
together with (nthcdr LIST)."
  (let* ((prevcdr (nthcdr (- n 1) list))
         (tail (cdr prevcdr)))
    (setf (cdr prevcdr) nil)
    (values list tail)))

(defmacro! popn (n place)
  "Set place to its nthcdr, and return the list we cut off."
  `(multiple-value-bind (,g!head ,g!tail)
       (popn% ,n ,place)
     (setf ,place ,g!tail)
     ,g!head))

(defun map-on-car (fn alist)
  "Call FN on every CAR of ALIST, preserving the CDR."
  (mapcar (lambda (x)
            (cons (funcall fn (car x))
                  (cdr x)))
          alist))

(defun map-reverse (fn lst &optional acc)
  "Apply `fn' on `lst' and return in reverse order."
  (if (consp lst)
      (map-reverse fn (cdr lst) (cons (funcall fn (car lst)) acc))
      acc))

;;; building "constant" lists
(defun n-copies (n item &optional acc)
  "Create a list containing N times the ITEM."
  (if (<= n 0)
      acc
      (n-copies (- n 1) item (cons item acc))))

(defun foreach (item list)
  "Build a list with one `item' for every element of `list'."
  (map 'list (ilambda+ item) list))

(defun foreach1 (first item list)
  "As `foreach', but use `first' for the first element instead of
`item'."
  (list* first (foreach item (rest list))))

(defun unbox1 (x)
  "If `x' is a cons, take the `car', otherwise just return `x'. Like
`unbox', but we stop after the first iteration."
  (if (consp x)
      (car x)
      x))

(defun unbox (x)
  "Take the `car' until we get an atom."
  (if (consp x)
      (unbox (car x))
      x))

(defun odd-elements (sequence)
  "Select the first element, the third, ... of a `sequence' (i.e. all
element with odd index (starting from 1)."
  (let ((odd t))
    (remove-if (ilambda (x) (notf odd)) sequence)))

(declaim (inline zip))
(defun zip (&rest lists)
  "Create tuples (lists) of the matching elements of `lists'"
    (apply #'mapcar #'list lists))
