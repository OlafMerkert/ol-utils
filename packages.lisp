(in-package #:cl-user)

(defpackage #:ol-utils
  (:nicknames #:ol)
  (:use #:cl
        #:iterate)
  (:export #:keyw #:gethash/c #:mappend #:ilambda
           #:length=1 #:compose #:defmacro!
           #:symb #:lambda-form-p #:defconstant/g
           #:def-symbol-p #:make-queue #:fill-array
           #:last1 #:begins-with #:lrange
           #:append1 #:sort-hash-table #:aand
           #:defsymconstant #:test-symbol-start
           #:list->array #:acond #:undefmethoda
           #:group-by #:arange #:it #:index
           #:mrange #:alambda #:transpose-list
           #:dequeue #:bind-multi #:range
           #:list->gensyms #:alast #:aif
           #:swallow #:flatten
           #:udm #:defclass/f #:mkatom #:drop
           #:old #:nconc1 #:splitn #:collect
           #:string-replace-all #:starts-with
           #:with-gensyms! #:mkstr #:prod
           #:sum #:mlambda #:cumulative-sums
           #:matching-tree-nodes #:mklist #:split
           #:def-symbol-transform #:defmemfun
           #:minimise #:filter #:ew #:amrange
           #:indices #:compose/red #:reverse/n
           #:flatten1 #:copy #:notf
           #:defmacro/g! #:maximise #:compress
           #:shuffle #:assoc1a #:memoize
           #:assoc1 #:alternate #:group
           #:enqueue #:array->list #:self
           #:fill-array%
           #:square-multiply
           #:splice-in
           #:in
           #:ncond
           #:dotimes+
           #:split-last
           #:ensure-adjustable-array
           #:droplast
           #:start-app
           #:define-application
           #:ilambda+
           #:args->names
           #:popn
           #:popn%
           #:make-infinite-array
           #:inf-aref
           #:extend-infinite-array
           #:ignore-unbound
           #:lazy-array-map
           #:length=0
           #:symb+
           #:run-program
           #:funcall+thread
           #:n-copies
           #:map-on-car))

(defpackage #:ol-user
  (:use #:cl #:ol-utils
        #:iterate))
