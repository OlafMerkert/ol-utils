(in-package #:cl-user)

(defpackage #:ol-utils
  (:nicknames #:ol)
  (:use #:cl)
  (:export :KEYW :GETHASH/C :MAPPEND :ILAMBDA
           :LENGTH=1 :COMPOSE :DEFMACRO!
           :SYMB :LAMBDA-FORM-P :DEFCONSTANT/G
           :DEF-SYMBOL-P :MAKE-QUEUE :FILL-ARRAY
           :LAST1 :BEGINS-WITH :LRANGE
           :APPEND1 :SORT-HASH-TABLE :AAND
           :DEFSYMCONSTANT :TEST-SYMBOL-START
           :LIST->ARRAY :ACOND :UNDEFMETHODA
           :GROUP-BY :ARANGE :IT :INDEX
           :MRANGE :ALAMBDA :TRANSPOSE-LIST
           :DEQUEUE :BIND-MULTI :RANGE
           :LIST->GENSYMS :ALAST :AIF
           :MSWALLOW :SWALLOW :FLATTEN
           :UDM :DEFCLASS/F :MKATOM :DROP
           :OLD :NCONC1 :SPLITN :COLLECT
           :STRING-REPLACE-ALL :STARTS-WITH
           :WITH-GENSYMS! :MKSTR :PROD
           :SUM :MLAMBDA :CUMULATIVE-SUMS
           :MATCHING-TREE-NODES :MKLIST :SPLIT
           :DEF-SYMBOL-TRANSFORM :DEFMEMFUN
           :MINIMISE :FILTER :EW :AMRANGE
           :INDICES :COMPOSE/RED :REVERSE/N
           :FLATTEN1 :COPY :NOTF
           :DEFMACRO/G! :MAXIMISE :COMPRESS
           :SHUFFLE :ASSOC1A :MEMOIZE
           :ASSOC1 :ALTERNATE :GROUP
           :ENQUEUE :ARRAY->LIST :SELF
           :FILL-ARRAY%))

(defpackage #:ol-user
  (:use #:cl #:ol-utils))
