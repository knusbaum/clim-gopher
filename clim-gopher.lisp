(in-package :clim-gopher)

(defgeneric present-gopher-line (line stream view))
(defmethod present-gopher-line ((line cl-gopher:gopher-line) stream view)
  (present line 'viewable-gopher-line :stream stream :view view))

(defmethod present-gopher-line ((line cl-gopher:info-message) stream view)
  (present line 'info :stream stream :view view))

(defmethod present-gopher-line ((line cl-gopher:error-code) stream view)
  (present line 'info :stream stream :view view))

(defmethod present-gopher-line ((line cl-gopher:html-file) stream view)
  (present line 'html-file :stream stream :view view))

(defmethod present-gopher-line ((line cl-gopher:search-line) stream view)
  (present line 'search :stream stream :view view))

(defun display-submenu-lines (lines stream)
  (formatting-table (stream :x-spacing '(3 :character))
    (loop for line in lines
       for view = (make-instance 'main-table-view)
       do (formatting-row (stream)
             (present-gopher-line line stream view)))))

(defun file-pathname (line)
  (with-slots (cl-gopher:selector) line
    (if (stringp (pathname-name cl-gopher:selector))
        (format nil "/tmp/clim-gopher_~a.~a" (pathname-name cl-gopher:selector)
                (pathname-type cl-gopher:selector))
        (format nil "/tmp/clim-gopher_unknown-~a" (random 100000)))))

(defun display-as-text (gl stream)
  (with-application-frame (frame)
    (let ((contents (cl-gopher:get-line-contents gl)))
      (cl-gopher:display-contents contents))
    (scroll-extent (find-pane-named frame 'main-display) 0 0)))

(defgeneric main-display-line (gl stream))

(defmethod main-display-line ((gl cl-gopher:gopher-line) stream)
  (let ((dl-name (file-pathname gl)))
    (cl-gopher:download-file dl-name gl)
    (format stream "Don't know how to display:~%~a~%File downloaded at:~a~%"
            gl dl-name)))

(defmethod main-display-line ((gl cl-gopher:submenu) stream)
  (handler-case
      (let ((lines (cl-gopher:lines (cl-gopher:get-line-contents gl))))
        (display-submenu-lines lines stream))
    (error (e) (display-as-text gl stream))))

(defmethod main-display-line ((gl cl-gopher:search-line) stream)
  (let ((lines (cl-gopher:lines (cl-gopher:get-line-contents gl))))
    (display-submenu-lines lines stream)))

(defun display-image (image stream)
  (let ((dl-name (file-pathname image)))
    (cl-gopher:download-file dl-name image)
    (handler-case
        (with-application-frame (frame)
          (let ((pattern (make-pattern-from-bitmap-file dl-name :format (get-type dl-name))))
            (with-room-for-graphics ()
              (draw-pattern* stream pattern 0 0))))
      (clim-extensions:unsupported-bitmap-format (e)
        (format stream "Error: ~a~%" e)
        (format stream "You can find the file at: ~a~%" dl-name)
        nil))))

(defmethod main-display-line ((gl cl-gopher:image) stream)
  (display-image gl stream))

(defmethod main-display-line ((gl cl-gopher:png) stream)
  (display-image gl stream))

(defmethod main-display-line ((gl cl-gopher:gif) stream)
  (display-image gl stream))

(defmethod main-display-line ((gl cl-gopher:text-file) stream)
  (display-as-text gl stream))

(defun display-main (frame stream)
  (let ((current (car (history frame))))
    (clim:with-drawing-options (stream :text-style (make-text-style nil :italic :large))
      (present current 'gopher-line :stream stream)
      (format stream "~%~%"))
    (handler-case
        (main-display-line current stream)
      (error (e)
        (format stream "Failed to Display file:~%")
        (present current 'viewable-gopher-line)
        (format stream "~%Error: ~a~%" e)))))

(defun display-history (frame stream)
  (clim:with-drawing-options (stream :text-style (make-text-style nil nil :large))
    (format stream "HISTORY~%"))
  (display-submenu-lines (history frame) stream))

(defun display-bookmarks (frame stream)
  (clim:with-drawing-options (stream :text-style (make-text-style nil nil :large))
    (format stream "BOOKMARKS~%"))
  (display-submenu-lines (bookmarks (bookmarks frame)) stream))

(define-application-frame gopher ()
  ((history :initform nil :accessor history)
   (show-uri :initform nil :initarg :show-uri :accessor show-uri)
   (bookmarks :initform (load-bookmarks) :accessor bookmarks))
  (:panes
   (main-display
    :application
    :display-function 'display-main
    :display-time t
    :scroll-bars t
    :text-style (make-text-style :fix nil nil))
   (history-display
    :application
    :display-function 'display-history
    :display-time t
    :scroll-bars t)
   (bookmarks-display
    :application
    :display-function 'display-bookmarks
    :display-time t
    :scroll-bars t))
  (:command-definer define-gopher-command)
  (:menu-bar t)
  (:layouts
   (default main-display)
   (history (horizontally ()
              (2/3 main-display)
              (1/3 history-display)))
   (bookmarks (horizontally ()
                 (2/3 main-display)
                 (1/3 bookmarks-display)))))

;;; Redisplay machinery
(defclass main-redisplay-event (window-manager-event)
  ((item :initarg :item :accessor item)))

(defmethod handle-event ((frame gopher) (event main-redisplay-event))
  (with-application-frame (frame)
    (let ((main-pane (find-pane-named frame 'main-display))
          (history-pane (find-pane-named frame 'history-display))
          (bookmarks-pane (find-pane-named frame 'bookmarks-display)))

      (when main-pane
        (setf (pane-needs-redisplay main-pane) t)
        (redisplay-frame-pane frame main-pane))

      (when history-pane
        (setf (pane-needs-redisplay history-pane) t)
        (redisplay-frame-pane frame history-pane))

      (when bookmarks-pane
        (setf (pane-needs-redisplay bookmarks-pane) t)
        (redisplay-frame-pane frame bookmarks-pane)))))

(defun perform-main-redisplay (gopher-app)
  (queue-event (frame-top-level-sheet gopher-app)
               (make-instance 'main-redisplay-event
                              :sheet gopher-app)))

;;; Menu Commands
(define-gopher-command (com-back :name t :menu t) ()
  (with-application-frame (frame)
    (when (> (length (history frame)) 1)
      (pop (history frame)))
    (perform-main-redisplay frame)))

(define-gopher-command (com-refresh :name t :menu t) ()
  (with-application-frame (frame)
    (perform-main-redisplay frame)))

(define-gopher-command (com-history :name t :menu t) ()
  (with-application-frame (frame)
    (case (frame-current-layout frame)
      (history (setf (frame-current-layout frame) 'default))
      (t (setf (frame-current-layout frame) 'history)))
    (perform-main-redisplay frame)))

(define-gopher-command (com-bookmarks :name t :menu t) ()
  (with-application-frame (frame)
    (case (frame-current-layout frame)
      (bookmarks (setf (frame-current-layout frame) 'default))
      (t (setf (frame-current-layout frame) 'bookmarks)))
    (perform-main-redisplay frame)))

(define-gopher-command (com-bookmark-this-page :name t :menu t) ()
  (with-application-frame (frame)
    (add-bookmark (bookmarks frame) (car (history frame)))
    (perform-main-redisplay frame)))

(define-gopher-command (com-toggle-uri-display :name t :menu t) ()
  (with-application-frame (frame)
    (setf (show-uri frame) (not (show-uri frame)))
    (perform-main-redisplay frame)))

;;; Element commands
(define-gopher-command (com-go-line :name t) ((viewable-gopher-line 'viewable-gopher-line))
  (with-application-frame (frame)
    (push viewable-gopher-line (history frame))
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator go-line
    (viewable-gopher-line com-go-line gopher
                 :gesture :select		;command activated with left-click on a node
                 :menu t)               ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-add-bookmark :name t) ((clickable-gopher-line 'clickable-gopher-line))
  (with-application-frame (frame)
    (add-bookmark (bookmarks frame) clickable-gopher-line)
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator add-bookmark
    (clickable-gopher-line com-add-bookmark gopher
            :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-remove-bookmark :name t) ((clickable-gopher-line 'clickable-gopher-line))
  (with-application-frame (frame)
    (remove-bookmark (bookmarks frame) clickable-gopher-line)
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator remove-bookmark
    (clickable-gopher-line com-remove-bookmark gopher
            :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-search :name t) ((search 'search))
  (with-application-frame (frame)
    (let ((mod-search (cl-gopher:copy-gopher-line search))
          (search-terms
           (accepting-values (t :own-window t)
             (cl-gopher:display-line search :stream t :include-newline t)
             (accept 'string :prompt "search"))))
      (when (not (equal search-terms ""))
        (setf (cl-gopher:terms mod-search)
              search-terms))
      (push mod-search (history frame))
      (perform-main-redisplay frame))))

(define-presentation-to-command-translator search
    (search com-search gopher
            :gesture :select		;command activated with left-click on a node
            :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-go-html :name t) ((html-file 'html-file))
  (with-application-frame (frame)
    (handler-case
        (trivial-open-browser:open-browser (subseq (cl-gopher:selector html-file) 4))
      (uiop:subprocess-error (e) nil))))

(define-presentation-to-command-translator go-html
    (html-file com-go-html gopher
               :gesture :select		;command activated with left-click on a node
               :menu t)             ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-go-path :name t :menu t) ()
  (let* ((path
          (accepting-values (t :own-window t)
                            (accept 'string :prompt "path")))

         (gopher-line (cl-gopher:parse-gopher-uri path)))
    (with-application-frame (frame)
      (push gopher-line (history frame))
      (perform-main-redisplay frame))))

(defvar *app*)
(defun browser (&key separate-thread (url "gopher://gopher.floodgap.com"))
  (setf *app* (clim:make-application-frame 'clim-gopher::gopher
                                           :width 1024
                                           :height 768))
  (setf (history *app*) (list (cl-gopher:parse-gopher-uri url)))
  (if separate-thread
      (clim-sys:make-process (lambda () (clim:run-frame-top-level *app*))
                             :name "Gopher Application")
      (clim:run-frame-top-level *app*)))
