(in-package :qt-utils)

(export '(qlayout
          make-qapp))

(defun same-symbol-name (s1 s2)
  "Teste, ob zwei Symbole die gleichen Symbolnamen haben (ignoriert
Paketzugehörigkeit)."
  (string-equal (symbol-name s1)
                (symbol-name s2)))

(defmacro! qlayout (widget type &body widgets)
  "Versehe das widget mit einem Layout und füge gleich die widgets hinzu."
  `(let ((,g!layout (make-qinstance ,(symb type '-layout))))
     ;; Verschiedene Anordnung für verschiedene Layout
     ,@(cond 
        ((or (same-symbol-name type :h-box)
             (same-symbol-name type :v-box))
         (mapcar (lambda (w)
                   (if (eql w :stretch)
                       `(q add-stretch ,g!layout)
                       `(q add-widget ,g!layout ,w)))
                 widgets))
        ((same-symbol-name type :form)
         (mapcar (lambda (w)
                   `(q add-row ,g!layout ,(first w) ,(second w)))
                 (group widgets 2)))
        ((same-symbol-name type :grid)
         (mapcar (lambda (i row)
                   (mapcar (lambda (j w)
                             `(q add-widget ,g!layout ,w ,i ,j))
                           (lrange row) (mklist row)))
                 (lrange widgets) widgets)))
     ;; ordne layout dem widget zu
     (setf (q layout ,widget) ,g!layout))
  `(qnew ,(mkstr type :-layout)
         ,@(mapcar #`(:add-widget ,@(mklist a1))
                   widgets)))

(defun make-qapp ()
  "Starte Qt Anwendung."
  (setf qt-user:*application*
        (make-qapplication)))
