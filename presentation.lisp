(in-package :clim-gopher)

;;; Presentation Code
(defclass main-table-view (textual-view) ())

(defun display-type (line stream)
  (let ((icon (icon-for (cl-gopher:line-type line))))
    (if icon
        (with-room-for-graphics ()
          (draw-design stream icon))
        (format stream "~a" (string-downcase (cl-gopher:line-type line))))))

(define-presentation-type gopher-line () :inherit-from '((string)
                                                         :description "gopher line"))

(define-presentation-method present (gopher-line (type gopher-line) stream
                                                 (view textual-view)
                                                 &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a[~a]"
          (cl-gopher:display-string gopher-line)
          (cl-gopher:uri-for-gopher-line gopher-line)))

(define-presentation-type clickable-gopher-line () :inherit-from '((gopher-line)
                                                         :description "gopher line"))

(define-presentation-type viewable-gopher-line () :inherit-from '((clickable-gopher-line)
                                                         :description "viewable gopher line"))

(define-presentation-method present (viewable-gopher-line (type viewable-gopher-line) stream
                                                 (view main-table-view)
                                                 &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type viewable-gopher-line stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher:display-string viewable-gopher-line)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "~a" (cl-gopher:uri-for-gopher-line viewable-gopher-line))))))

(define-presentation-type search () :inherit-from '((clickable-gopher-line)
                                                    :description "search"))

(define-presentation-method present (search (type search) stream
                                            (view main-table-view)
                                            &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type search stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher:display-string search)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "~a~c~a"
                (cl-gopher:uri-for-gopher-line search)
                #\Tab
                (cl-gopher:terms search))))))

(define-presentation-type info () :inherit-from '((gopher-line)
                                                  :description "info"))

(define-presentation-method present (info (type info) stream
                                          (view main-table-view)
                                          &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream ""))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher:display-string info)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "")))))

(define-presentation-type html-file () :inherit-from '((clickable-gopher-line)
                                                       :description "html-file"))

(define-presentation-method present (html-file (type html-file) stream
                                               (view main-table-view)
                                               &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type html-file stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher:display-string html-file)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "~a" (cl-gopher:selector html-file))))))

(define-presentation-type unknown () :inherit-from '((gopher-line)
                                                  :description "unknown"))

(define-presentation-method present (unknown (type unknown) stream
                                          (view main-table-view)
                                          &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type unknown stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher:display-string unknown)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "")))))
