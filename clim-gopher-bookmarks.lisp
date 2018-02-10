(in-package :clim-gopher)

;;; None of this is totally functional or integrated yet.

(defclass bookmarks ()
  ((fname :initarg :fname :accessor fname)
   (bookmarks :initarg :bookmarks :initform nil :accessor bookmarks)))

(defun read-bookmarks (fname)
  (handler-case
      (with-open-file (is fname)
        (let ((gopher-lines (read is)))
          (make-instance 'bookmarks
                         :fname fname
                         :bookmarks (cl-gopher::unmarshall-gopher-lines gopher-lines))))
    (file-error (e) (make-instance 'bookmarks
                         :fname fname))))

(defun add-bookmark (bookmarks gl)
  (push gl (bookmarks bookmarks))
  (with-open-file (os (fname bookmarks) :direction :output :if-exists :supersede)
    (write (cl-gopher::marshall-gopher-lines (bookmarks bookmarks)) :stream os)))



(define-application-frame bookmark-frame ()
  ((bookmarks :initform nil :accessor bookmarks))
  (:panes
   (bookmark-display
    :application
    :display-function 'bookmark-display
    :display-time t)
   (int :interactor))
  (:command-definer define-bookmark-command)
  (:layouts
   (default (vertically ()
              (99/100 main-display)
              (1/100 int)))))
