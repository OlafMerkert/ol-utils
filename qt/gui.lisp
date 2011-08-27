(in-package :qt-utils)

(export '(qlayout
          make-qapp
          run-qapp))

(defmacro! qlayout (widget type &body widgets)
  "Versehe das widget mit einem Layout und füge gleich die widgets hinzu."
  `(let ((,g!layout (make-qinstance ',(symb type '-layout))))
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
     (setf (q layout ,widget) ,g!layout)))

(defun make-qapp ()
  "Lege Qt Anwendung an."
  (setf qt-user:*application*
        (make-qapplication)))

(defun run-qapp ()
  "Starte Qt Anwendung."
  (q exec qt-user:*application*))
