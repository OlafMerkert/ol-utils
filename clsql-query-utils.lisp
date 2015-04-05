(in-package :clsql-helpers)

(in-readtable clsql-readtable)

(defun append-to-select (field tree month next-month)
  "add a :where clause filtering for `time' to be in the range [month,
next-month[ for every select statement in the tree. Currently does not
support nested selects -- probably not needed."
  (cond ((atom tree) tree)
        ((and (consp tree) (member (car tree) '(select)))
         ;; no further recursion, don't expect nested selects
         (append tree `(:where [and [<= ,month ,field] [< ,field ,next-month]])))
        (t                              ; usual tree recursion
         (cons (append-to-select field (car tree) month next-month)
               (append-to-select field (cdr tree) month next-month)))))

(defmacro! with-month-query ((field o!month &optional next-month) &body body)
  "If `month' evaluates non-nil, add a filter to every embedded SQL
query which selects only entries with `field' between `month' and
`next-month' (which defaults to the next month after `month')."
  `(if ,g!month
       (let1 (,g!next-month (or ,next-month
                                (and ,g!month (local-time:timestamp+ ,g!month 1 :month))))
         ;; TODO perhaps here we better use `macrolet'
         ,@(append-to-select field body g!month g!next-month))
       (progn ,@body)))
