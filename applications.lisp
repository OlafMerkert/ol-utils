(in-package :ol-utils)

(defgeneric start-app (id)
  (:documentation "Start a registered (and installed) cl application."))

(defmacro define-application (id system package function)
  "Define a starter function for ID that loads a SYSTEM, then calls
FUNCTION in PACKAGE. ID ought to be a keyword symbol, on the other
arguments we call mkstr, so use symbols or strings as you prefer."
  `(defmethod start-app ((id (eql ,id)))
     (ql:quickload ,(mkstr system))
     (funcall
      (intern ,(mkstr function)
              ,(mkstr package)))))

(define-application :tvs tv-series-status tvs-clim tv-series-display)
