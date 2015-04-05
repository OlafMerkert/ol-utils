(defpackage :ol-date-utils
  (:use :cl :ol :local-time)
  (:shadow :encode-timestamp)
  (:export
   :encode-date
   :print-date
   :parse-date
   :from-today
   :define-parse-date
   :year
   :month
   :day
   :print-date-and-time
   :print-date/reverse
   :parse-date/us
   :encode-timestamp))

(in-package :ol-date-utils)

(defun encode-timestamp
    (&key (nsec 0) (sec 0) (minute 0) (hour 0) (day 1) (month 1) (year 1970)
          (timezone local-time:*default-timezone*) offset into)
  "mimic `local-time:encode-timestamp', but use keyword arguments
for everything, with the obvious defaults."
  (local-time:encode-timestamp nsec sec minute hour day month year :timezone timezone :offset offset :into into))

(defun encode-date (day month &optional year)
  (encode-timestamp :day day :month  month
                    ;; support two digit numbers for the year
                    :year (cond ((null year) (timestamp-year (today)))
                          ((< year 35) (+ 2000 year))
                          ((< year 100) (+ 1900 year))
                          (t year))
                    ;; use timezone UTC, so sec-of is 0
                    :timezone +utc-zone+))

(defun print-date (date &optional stream)
  (format stream
          "~2,'0D.~2,'0D.~4,'0D"
          (timestamp-day   date)
          (timestamp-month date)
          (timestamp-year  date)))

(defun print-date/reverse (date &optional stream)
  (format stream
          "~4,'0D-~2,'0D-~2,'0D"
          (timestamp-year  date)
          (timestamp-month date)
          (timestamp-day   date)))

(defun print-date-and-time (date &optional stream)
  (format stream
          "~2,'0D.~2,'0D.~4,'0D ~2,'0D:~2,'0D"
          (timestamp-day    date)
          (timestamp-month  date)
          (timestamp-year   date)
          (timestamp-hour   date)
          (timestamp-minute date)))

(defun ->integer (obj)
  (cond ((stringp obj) (parse-integer obj))
        (t obj)))

(defmacro! define-parse-date (name separator parts &optional doc)
  `(defun ,name (,g!string)
     ,doc
     (let ((,g!parts (split-sequence:split-sequence ,separator ,g!string
                                                    :remove-empty-subseqs t)))
       (when ,g!parts
         (destructuring-bind ,parts ,g!parts
           (encode-date
            (->integer day)
            (->integer month)
            (->integer year)))))))

(define-parse-date parse-date #\. (day month &optional year)
                   "Parse a date of form DD.MM.YYYY or DD.MM.")

(define-parse-date parse-date/us #\- (year month day)
                   "Parse a date of form YYYY-MM-DD.")

(defun from-today (&optional (nr-of-days 0) evening)
  "Generate a date offset from today by NR-OF-DAYS. If EVENING,
increase offset by 1."
  (timestamp+ (today) (if evening (+ 1 nr-of-days) nr-of-days) :day ))

(defun shift-dates (ref-from ref-to &rest other-dates)
  "Return a list of the OTHER-DATES shifted by the same time
difference as REF-FROM and REF-TO."
  (let ((diff (timestamp-difference ref-to ref-from)))
    (mapcar (lambda (x)
              (timestamp+ x diff :sec))
            other-dates)))
