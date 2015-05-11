(in-package :ol-utils)

;; faster loading with quicklisp
#+quicklisp (defalias ql ql:quickload)
#+quicklisp (defun ql+ (system)
              (check-type system symbol)
              (unless (and (asdf:find-system system nil)
                           (asdf:component-loaded-p system))
                (ql:quickload system)))


(defgeneric start-app (id &rest args)
  (:documentation "Start a registered (and installed) cl application."))

(defmacro define-application (id system package function &optional documentation)
  "Define a starter function for `id' that loads a `system', then
calls `function' in `package'. `id' ought to be a keyword symbol, on
the other arguments we call `mkstr' anyway, so use symbols or strings
as you prefer. Additionally, you may provide a description or
`documentation'."
  `(progn
     (pushnew (list ,id ,documentation) *application-registry* :key #'car)
     (defmethod start-app ((id (eql ,id)) &rest args)
       ,documentation
      (ql:quickload ,(mkstr system))
      (apply
       (intern ,(mkstr function)
               ,(mkstr package))
       args))))

(defvar *application-registry* (list (list :list "Show all available applications.")))

(defmethod start-app ((id (eql :list)) &rest args)
  (declare (ignore args))
  (le1 (stream *standard-output*)
    (map nil (lambda (app)
               (format stream "~&~S~15T~A~%" (first app) (second app)))
         *application-registry*)))

(define-application :tvs tv-series-status-clim tvs-clim tv-series-display
                    "Download and display first airings of the episodes of selected TV series.")

(define-application :tvs-web tv-series-status-web tvs-web start-server-and-open
                    "Start a webserver to display first airings of the episodes of selected TV series.")

(define-application :mi math-interactor math-interactor math-interactor
                    "A CLIM interface to a simple CAS system built onto CLOS.")

(define-application :cpi complex-plane-interactor complex-plane-geometry complex-plane-interactor
                    "A CLIM interface for manipulating circles and lines on the complex plane.")

(define-application :imdbg download-helpers download-imdb-gallery download-gallery-interactive
                    "A REPL application to download all images from a selected gallery.")

(define-application :sa simple-accounting simple-accounting-interface simple-account
                    "A GTK application for recording expenses in a SQLite database.")

(define-application :bm bibtex-manager bibtex-manager/clim-ui manager-ui
                    "A CLIM application for retrieving and storing BibTeX entries from mathscinet, and associating them to a local document library.")
