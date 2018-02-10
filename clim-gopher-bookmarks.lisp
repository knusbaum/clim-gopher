(in-package :clim-gopher)

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
              (1/10
               (horizontally ()
                 (make-button "Back" (lambda ()
                                       (with-application-frame (frame)

                                         (cond
                                           ((display-image frame)
                                            (setf (display-image frame) nil))

                                           ((display-file frame)
                                            (setf (display-file frame) nil))

                                           (t
                                            (let ((curr (pop (history frame)))
                                                  (prev (car (history frame))))
                                              (if prev
                                                  (setf (curr-lines frame)
                                                        (cl-gopher:gopher-get-directory
                                                         (cl-gopher::hostname prev)
                                                         (cl-gopher::port prev)
                                                         (cl-gopher::selector prev)))
                                                  (push curr (history frame))))))

                                         (perform-main-redisplay frame))))
                 (make-button "Refresh" (lambda ()
                                          (with-application-frame (frame)
                                            (let ((curr (car (history frame))))
                                              (setf (curr-lines frame)
                                                    (cl-gopher:gopher-get-directory
                                                     (cl-gopher::hostname curr)
                                                     (cl-gopher::port curr)
                                                     (cl-gopher::selector curr)))
                                              (perform-main-redisplay frame)))))
                 (make-button "History" (lambda ()
                                          (with-application-frame (frame)
                                            (cond
                                              ((display-image frame)
                                               (setf (display-image frame) nil))
                                              
                                              ((display-file frame)
                                               (setf (display-file frame) nil)))

                                            (setf (curr-lines frame)
                                                  (history frame))

;                                            (push nil (history frame))
                                            
                                            (perform-main-redisplay frame))))))
              (9/10 (vertically ()
                      (99/100 main-display)
                      (1/100 int)))))))
