(in-package :ol-utils)

(export '(delay
          force
          make-pipe
          empty-pipe
          head
          tail
          pipe-elt
          filter-pipe
          map-pipe
          append-pipes
          mappend-pipes
          combine-all-pipes))

;;; Lazy evaluation
;;; from AIP

(defstruct delay (value nil) (function nil))

(defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :function (lambda () ,@body)))

(defun force (x)
  "Find the value of x, by computing it if it is a delay."
  (if (not (delay-p x))
      x
      (progn
        (when (delay-function x)
          (setf (delay-value x)    (funcall (delay-function x))
                (delay-function x) nil))
        (delay-value x))))

;;; based on it: pipes, also from AIP
(defmacro make-pipe (head tail)
  "Create a pipe by evaluating head and delaying tail."
  `(cons ,head (delay ,tail)))

(defconstant empty-pipe nil)

(defun head (pipe) (first pipe))
(defun tail (pipe) (force (rest pipe)))

(defun pipe-elt (pipe i)
  "The i-th element of a pipe, 0-based."
  (if (= i 0)
      (head pipe)
      (pipe-elt (tail pipe) (- i 1))))

(defun filter-pipe (pred pipe)
  "Keep only items in pipe satisfying pred."
  (if (funcall pred (head pipe))
      (make-pipe (head pipe)
                 (filter-pipe pred (tail pipe)))
      (filter-pipe pred (tail pipe))))

(defun map-pipe (fn pipe)
  "Map fn over pipe, delaying all but the first fn call."
  (if (eq pipe empty-pipe)
      empty-pipe
      (make-pipe (funcall fn (head pipe))
                 (map-pipe fn (tail pipe)))))

(defun append-pipes (x y)
  "Return a pipe that appends the elements of x and y."
  (if (eq x empty-pipe)
      y
      (make-pipe (head x)
                 (append-pipes (tail x) y))))

(defun mappend-pipe (fn pipe)
  "Lazily map fn over pipe, appending results."
  (if (eq pipe empty-pipe)
      empty-pipe
      (let ((x (funcall fn (head pipe))))
        (make-pipe (head x)
                   (append-pipes (tail x)
                                 (mappend-pipe fn (tail pipe)))))))

(defun combine-all-pipes (xpipe ypipe)
  "Return a pipe of pipes formed by appending a y to an x"
  ;; In other words, form the cartesian product.
  (mappend-pipe
   (lambda (y)
     (map-pipe (lambda (x) (append-pipes x y))
               xpipe))
   ypipe))
