(in-package #:ol-utils)

(export '(sum prod))

;; TODO capture the abstraction
(defmacro! sum ((var o!start &optional o!end (o!step 1)) expr)
  `(progn
     (unless ,g!end
       (setf ,g!end   ,g!start
             ,g!start 0))
     (unless (plusp (* ,g!step (- ,g!end ,g!start)))
       (error "Invalid range from ~A to ~A by ~A" ,g!start ,g!end ,g!step))
     (do ((,var ,g!start (+ ,g!step ,var))
          (,g!sum 0))
         ((>= ,var ,g!end) ,g!sum)
       (incf ,g!sum
             ,expr))))

(defmacro! prod ((var o!start &optional o!end (o!step 1)) expr)
  `(progn
     (unless ,g!end
       (setf ,g!end   ,g!start
             ,g!start 0))
     (unless (plusp (* ,g!step (- ,g!end ,g!start)))
       (error "Invalid range from ~A to ~A by ~A" ,g!start ,g!end ,g!step))
     (do ((,var ,g!start (+ ,g!step ,var))
          (,g!sum 1))
         ((>= ,var ,g!end) ,g!sum)
       (setf ,g!sum (* ,g!sum
                       ,expr)))))

;; TODO Terminierungsbedingung fuer Schleifen bisher nur fuer
;; aufsteigende, betrifft auch *range Funktionen.