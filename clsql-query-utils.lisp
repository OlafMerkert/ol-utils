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

;; todo move timestamp converter to date-utils?
(defgeneric to-timestamp (obj))

(defmethod to-timestamp ((integer integer))
  (local-time:universal-to-timestamp integer))

(defmethod to-timestamp ((timestamp local-time:timestamp))
  timestamp)

(defmethod to-timestamp ((date clsql-sys:date))
  (mvbind (day month year) (clsql-sys:decode-date date)
    (ol-date-utils:encode-date day month year)))

(defmethod to-timestamp ((time clsql-sys:wall-time))
  (mvbind (usec second minute hour day month year) (decode-time time)
    (ol-date-utils:encode-timestamp
     ;; todo pass also `usec'
     :sec second :minute minute :hour hour :day day :month month :year year)))

(defmethod to-timestamp ((time-string string))
  (local-time:parse-timestring time-string))

(defmethod clsql-sys::database-output-sql ((self local-time:timestamp) database)
  "Handle `timestamp's from `local-time' transparently in clsql."
  (clsql-sys::database-output-sql
   (local-time:timestamp-to-universal self) database))

(defun first-month (table &optional (field [time]) last)
  "Find the first month with log entries."
  (let1 (ts (to-timestamp
             (caar (select field :from table
                           :order-by `((,field ,(if last :desc :asc)))
                           :limit 1)) ))
    (values (local-time:timestamp-year ts)
            (local-time:timestamp-month ts)
            ts)))

(defun last-month (table &optional (field [time]) first)
  "Find the last month with log entries."
  (first-month table field (not first)))

(defun months-table (table field &key (first (multiple-value-list (first-month table field)))
                                      (last (multiple-value-list (last-month table field))))
  "Produce a list of the month where there might be log
entries (simply the range between `first' and `last'."
  ;; count down the months from `last' to `first'
  (do ((year (car last))
       (month (cadr last))
       (year1 (car first))
       (month1 (cadr first))
       timestamps)
      ((or (< year year1)
           (and (= year year1) (< month month1)))
       timestamps)
    (push (ol-date-utils:encode-timestamp :year year :month month) timestamps)
    (decf month)
    (when (zerop month)
      (decf year)
      (setf month 12))))
