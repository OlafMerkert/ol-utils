(in-package :ol-utils)

(defun mapc-lines-of-file% (function file)
  "mapc `function' over the lines of `file'."
  (with-open-file (str file)
    (iter (for line next (read-line str nil :eof))
          (until (eq line :eof))
          (funcall function line))))

(defun mapc-lines-of-file (function file &key (test (constantly t)))
  "mapc `function' over the lines of `file', on which `test' is true."
  (mapc-lines-of-file%
   (lambda (line) (when (funcall test line)
               (funcall function line)))
   file))

(defun lines-of-file (file &key (test (constantly t)))
  "Return a list of all the lines in `file' on which `test' is true."
  (let (lines)
    (mapc-lines-of-file%
     (lambda (line) (when (funcall test line) (push line lines)))
     file)
    (nreverse lines)))

(defun map-lines-of-file (function file &key (test (constantly t)))
  "Map `function' over all lines of `file', on which `test' is true."
  (let (mapped-lines)
    (mapc-lines-of-file%
     (lambda (line) (when (funcall test line)
                 (push (funcall function line) mapped-lines)))
     file)
    (nreverse mapped-lines)))

(defun read-file-1 (pathname)
  "Read the first sexp in the given file."
  (with-open-file (stream pathname :if-does-not-exist :error)
    (read stream)))
