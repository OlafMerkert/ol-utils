(in-package #:cl-user)

(defpackage #:ol-utils
  (:nicknames #:ol)
  (:use #:cl
        #:iterate)
  (:import-from :named-readtables #:in-readtable)
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
           #:with-gensyms! #:mkstr #:cumulative-sums
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
           #:map-on-car
           #:clambda
           #:+memo-clear+
           #:+memo-container+
           #:memolabels
           #:memolambda
           #:memodefun
           #:memolambda/i
           #:memolabels/i
           #:memodefun/i
           #:+uncalculated+
           #:mfuncall
           #:memoize/i
           #:mvbind
           #:dbind
           #:lazy-arefs
           #:foreach
           #:foreach1
           #:unbox1
           #:unbox
           #:^
           #:_
           #:signcase
           #:defalias
           #:length=n
           #:lines-of-file
           #:mapc-lines-of-file
           #:class-name-of
           #:partition
           #:splitn/values
           #:pass-symbol
           #:table-clean-if
           #:sethash
           #:multi-dim-dotimes+
           #:fill-array/old
           #:make-array/fill
           #:make-array/fill%
           #:make-array/sparse
           #:peek
           #:pad-vector-front
           #:awhen
           #:until-t
           #:map-array
           #:map-array1
           #:sequence=
           #:defmacros!
           #:progress-event
           #:mappend-pipe
           #:defpar
           #:assoc1*
           #:alist-bind
           #:tree-find-if
           #:odd-elements
           #:remove*
           #:read-file-1
           #:length>0
           #:format-file-size
           #:plist-bind
           #:zip
           #:memoize/ignore-default
           #:filter*
           #:le1
           #:mvprog1
           #:appendf
           #:random-elt
           #:nprog1
           #:map-vector-2
           #:trim1-if
           #:trim1-if-not
           #:trim1
           #:let1
           #:table-values
           #:table-keys
           #:table->alist
           #:compose/names
           #:ql
           #:maphash-keys
           #:maphash-values
           #:string-join
           #:take
           #:labels1
           #:macrolet1
           #:ql+
           #:ol-readtable
           #:delay
           #:force
           #:make-pipe
           #:empty-pipe
           #:head
           #:tail
           #:pipe-elt
           #:filter-pipe
           #:map-pipe
           #:append-pipes
           #:mappend-pipes
           #:aprog1
           #:dbug
           #:create-standard-print-object
           #:lazy-aref
           #:make-lazy-array
           #:this
           #:lazy-array-array
           #:lazy-array-function
           #:lazy-array-finite
           #:lazy-array-default-value
           #:la
           #:lazy-array-take
           #:lazy-array-drop
           #:la-finite-test
           #:with-lazy-arefs
           #:la%
           #:make-nlazy-array
           #:nla
           #:nla%
           #:array-take
           #:array-drop
           #:nla-finite-test
           #:with-arefs
           #:map-tree
           #:map-tree-if
           #:in-readtable
           #:install-ol-read-macros))

(defpackage #:ol-user
  (:use #:cl #:ol-utils
        #:iterate))
