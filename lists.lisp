(in-package #:ol-utils)

(export '(mklist
          range mrange lrange
          alternate
          reverse/n
          drop
          split
          singleton-p))

(defun mklist (x)
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
  (range (length seq)))

(defun alternate (&rest lists)
  (case (car lists)
    ((:normal)
     (apply #'mapcan #'list (cdr lists)))
    ((:reverse)
     (apply #'mapcan #'list (reverse (cdr lists))))
    (t (apply #'mapcan #'list lists))))

(defun reverse/n (n list)
  "Verdrehe die Reihenfolge der ersten n Eintraege der Liste."
  (let ((a (subseq list 0 n))
        (b (subseq list n)))
    (nconc (nreverse a) b)))

(defun drop (n list)
  "Entferne den n-ten Eintrag aus der Liste."
  (nconc (subseq list 0 n)
         (subseq list (1+ n))))

(defun split (sep list)
  "Teile die Liste auf in Teile, die durch sep getrennt sind."
  (labels ((rec (remaining parts)
             (aif (position sep remaining)
                  (rec (nthcdr (1+ it) remaining)
                       (cons (subseq remaining 0 it) parts))
                  (cons (subseq remaining 0) parts))))
    (nreverse (rec list nil))))

(defun singleton-p (list)
  (and (listp list)
       (null (cdr list))))
