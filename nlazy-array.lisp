(in-package :ol-utils)

(export '(make-nlazy-array
          nla
          nla%
          array-take
          array-drop
          nla-finite-test
          with-arefs))

;; provide some of the convenience macros and functions for
;; lazy-array, but now for ordinary (1-Ð) arrays, enabling duality of
;; code.

(defun max% (&rest args)
  "The same as max, with nil treated as -infinity."
  (apply #'max (remove nil args)))

(defun min% (&rest args)
  "The same as min, with nil treated as +infinity."
  (apply #'max (remove nil args)))

(defmacro! make-nlazy-array ((&key start (index-var 'index) finite default-value)
                             &body fill-form)
  "START must be a list of the first entries of the array.  In
FILL-FORM, the generated actual array is bound to THIS, so you may
reference array elements of lower index, but never of higher.  The
index of the field to be filled is stored in a variable whose name you
provide with INDEX-VAR, by default just INDEX.

If FINITE exceeds the length of START, the remaining entries of the
array will be filled, so that is has length FINITE.  Otherwise, the
array will just contain the elements of START."
  (if (not finite)
      `(nla% nil ,@start)
      `(let* ((,g!finite ,finite)
              (this (make-array (max% ,(length start) ,g!finite) :initial-element ,default-value)))
         (setf ,@(mapcan #2`((aref this ,a1) ,a2) (lrange start) start))
         (when ,g!finite
           (loop
              for ,index-var from ,(length start) below ,g!finite do
                (setf (aref this ,index-var) (progn ,@fill-form))))
         this)))

(defun nla% (default &rest start)
  "Behaves exactly like NLA, only with the first parameter ignored."
  (declare (ignore default))
  (if (and (length=1 start)
           (arrayp (first start)))
      start
      (list->array start)))

(defun nla (&rest start)
  "If there is only parameter of type array given, return it.
Otherwise, create an array from all parameters."
  (apply #'nla% start))

(defun array-take (array n &optional (lazy-p t))
  "Extract the first N entries from the array.  LAZY-P is ignored and
  just for compatibility with lazy-array-take."
  (declare (ignore lazy-p))
  (subseq array 0 n))

(defun array-drop (array n)
  "Return a new array with the first N entries removed."
  (subseq array n))

(defmacro nla-finite-test (arrays &body body)
  "Evaluate BODY where each array stands for its length."
  `(let ,(mapcar #`(,a1 (length ,a1)) arrays)
     ,@body))

(defmacro! with-arefs ((o!object &rest accessors) &body body)
  "For each A in ACCESSORS, allow writing (A I) instead of (aref (A
  OBJECT) I). (should be SETF-able.)"
  (let ((acsyms (list->gensyms :accessor accessors)))
    `(let ,(mapcar #2`(,a1 (,a2 ,g!object))
                   acsyms accessors)
       (macrolet
           ,(mapcar #2`(,a2 (,g!index)
                            `(aref ,',a1 ,g!index))
                    acsyms accessors)
         ,@body))))
