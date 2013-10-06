(in-package :ol-utils)

(defgeneric start-app (id &rest args)
  (:documentation "Start a registered (and installed) cl application."))

(defmacro define-application (id system package function)
  "Define a starter function for ID that loads a SYSTEM, then calls
FUNCTION in PACKAGE. ID ought to be a keyword symbol, on the other
arguments we call mkstr, so use symbols or strings as you prefer."
  `(progn
     (pushnew ,id application-registry)
     (defmethod start-app ((id (eql ,id)) &rest args)
      (ql:quickload ,(mkstr system))
      (apply
       (intern ,(mkstr function)
               ,(mkstr package))
       args))))

(defvar application-registry (list :list))

(defmethod start-app ((id (eql :list)) &rest args)
  (declare (ignore args))
  application-registry)

(define-application :tvs tv-series-status tvs-clim tv-series-display)

(define-application :tvs-web tv-series-status tvs-web start-server-and-open)

(define-application :mi math-interactor math-interactor math-interactor)

(define-application :cpi complex-plane-interactor complex-plane-geometry complex-plane-interactor)

(define-application :imdbg download-helpers download-imdb-gallery download-gallery-interactive)
