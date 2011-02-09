(in-package #:ol-utils)

(export '(list->array array->list
          fill-array% fill-array old index indices))

(defun list->array (list)
  (let ((n (length list)))
    (values 
     (make-array n :initial-contents list)
     n)))

;; TODO Mehrdimensionale Arrays
(defun array->list (array)
  (loop for x across array collect x))

(defun fill-array% (dim index fill-spec array fill-fn)
  (cond ((null fill-spec)
         (let ((index (reverse index)))
           (setf #1=(apply #'aref array index)
                 (funcall fill-fn #1# index))))
        ((eq :fill (car fill-spec))
         (dotimes (i (array-dimension array dim))
           (fill-array% (1+ dim) (cons i index)
                        (cdr fill-spec) array fill-fn)))
        (t (fill-array% (1+ dim) (cons (car fill-spec) index)
                        (cdr fill-spec) array fill-fn))))

(defmacro fill-array (fill-spec array expr)
  `(progn
     (fill-array% 0 nil (list ,@fill-spec) ,array
               (lambda (old index)
                 (declare (ignorable old index))
                 ,expr))
     ,array))

(defmacro indices (names &body body)
  `(destructuring-bind ,(mklist names) index ,@body))