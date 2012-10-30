(defpackage :ol-date-utils
  (:use :cl :ol :local-time)
  (:export
   :encode-date
   :print-date
   :parse-date
   :from-today))

(in-package :ol-date-utils)

(defun encode-date (day month &optional year)
  (encode-timestamp 0 0 0 0 day month
                    ;; support two digit numbers for the year
                    (cond ((null year) (timestamp-year (today)))
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

(defun parse-date (string)
  "Parse a date of form DD.MM.YYYY or DD.MM."
  (let* ((parts     (split-sequence:split-sequence #\. string
                                                   :remove-empty-subseqs t))
         (int-parts (mapcar #'parse-integer parts)))
    (case (length int-parts)
      ((2 3)
       ;; TODO fancier validation, check that we only have digits in
       ;; the date.
       (apply #'encode-date int-parts)))))

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
