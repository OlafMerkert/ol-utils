(in-package #:ol-utils)

(export '(string-replace-all))

(defun string-replace-all (pattern string replacement)
  "Replace all exact occurrences of pattern in string with
replacement."
  (let ((l (length pattern)))
    (with-output-to-string (str)
      (labels ((repl (pos)
                 (aif (search pattern string :start2 pos)
                      (progn
                        (princ (subseq string pos it) str)
                        (princ replacement str)
                        (repl (+ it l)))
                      (princ (subseq string pos) str))))
        (repl 0)))))
