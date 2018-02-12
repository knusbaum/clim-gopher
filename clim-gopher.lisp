(in-package :clim-gopher)

(defun display-submenu-lines (lines stream)
  (formatting-table (stream :x-spacing '(3 :character))
    (loop for line in lines
       for view = (make-instance 'main-table-view)
       do (formatting-row (stream)
            (case (line-type line)
              (:info-message (present line 'info :stream stream :view view))
              (:error-code   (present line 'info :stream stream :view view))
              (:html-file    (present line 'html-file :stream stream :view view))
              (:search       (present line 'search :stream stream :view view))
              (t             (present line 'viewable-gopher-line :stream stream :view view)))))))

(defun display-submenu (submenu stream)
  (let ((lines (gopher-get-submenu submenu)))
    (display-submenu-lines lines stream)))

(defun display-search (search stream)
  (let ((lines (gopher-do-search search)))
    (display-submenu-lines lines stream)))

(defun file-pathname (image)
  (if (stringp (pathname-name (selector image)))
      (format nil "/tmp/clim-gopher_~a.~a" (pathname-name (selector image))
              (pathname-type (selector image)))
      (format nil "/tmp/clim-gopher_unknown-~a" (random 100000))))

(defun display-image (image stream)
  (let ((dl-name (file-pathname image)))
    (download-file dl-name
                   (hostname image)
                   (port image)
                   (selector image))
    (handler-case
        (with-application-frame (frame)
          (let ((pattern (make-pattern-from-bitmap-file dl-name :format (get-type dl-name))))
            (with-room-for-graphics ()
              (draw-pattern* stream pattern 0 0))))
      (clim-extensions:unsupported-bitmap-format (e)
        (format stream "Error: ~a~%" e)
        (format stream "You can find the file at: ~a~%" dl-name)
        nil))))

(defun display-as-text (gl stream)
  (let ((lines (text-file-get-lines gl)))
    (with-application-frame (frame)
      (loop for line in lines
         do (format stream "~a~%" line))
      (scroll-extent (find-pane-named frame 'main-display) 0 0))))

(defun display-main (frame stream)
  (let ((current (car (history frame))))
    (clim:with-drawing-options (stream :text-style (make-text-style nil :italic :large))
      (present current 'gopher-line :stream stream)
      (format stream "~%~%"))
    (handler-case
        (case (line-type current)
          (:submenu (handler-case (display-submenu current stream)
                      (error (e) (display-as-text current stream))))
          (:search (display-search current stream))
          (:text-file (display-as-text current stream))
          (:image (display-image current stream))
          (:png (display-image current stream))
          (:gif (display-image current stream))
          (t (let ((dl-name (file-pathname current)))
               (download-file dl-name
                              (hostname current)
                              (port current)
                              (selector current))
               (format stream "Don't know how to display:~%~a~%File downloaded at:~a~%"
                       current dl-name))))
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
    (let ((mod-search (copy-gopher-line search))
          (search-terms
           (accepting-values (t :own-window t)
             (format t "~a~%" (display-string search))
             (accept 'string :prompt "search"))))
      (when (not (equal search-terms ""))
        (setf (terms mod-search)
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
        (trivial-open-browser:open-browser (subseq (selector html-file) 4))
      (uiop:subprocess-error (e) nil))))

(define-presentation-to-command-translator go-html
    (html-file com-go-html gopher
               :gesture :select		;command activated with left-click on a node
               :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-go-path :name t :menu t) ()
  (let* ((path
          (accepting-values (t :own-window t)
            (accept 'string :prompt "path")))
         (uri (if (and (>= (length path) 9) (equal "gopher://" (subseq path 0 9)))
                 (quri:uri path)
                 (quri:uri (format nil "gopher://~a" path)))))
    (with-application-frame (frame)
      (push
       (make-instance 'gopher-line
                      :line-type :submenu
                      :display-string "???"
                      :selector (quri:uri-path uri)
                      :hostname (quri:uri-host uri)
                      :port (or (quri:uri-port uri) 70))
       (history frame))
      (perform-main-redisplay frame))))

(defvar *app*)
(defun browser (&key separate-thread)
  (setf *app* (clim:make-application-frame 'clim-gopher::gopher
                                           :width 1024
                                           :height 768))
  (setf (history *app*) (list (make-instance 'gopher-line
                                             :line-type :submenu
                                             :display-string "Home"
                                             :selector "/"
                                             :hostname "gopher.floodgap.com"
                                             :port 70)))
  (if separate-thread
      (clim-sys:make-process (lambda () (clim:run-frame-top-level *app*))
                             :name "Gopher Application")
      (clim:run-frame-top-level *app*)))
