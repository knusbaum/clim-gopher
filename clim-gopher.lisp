(in-package :clim-gopher)

(defclass special-textual-view (textual-view) ())

(define-presentation-type line-type () :inherit-from '((string)
                                                       :description "gopher type"))

(defun display-type (line stream)
  (let ((icon (icon-for (cl-gopher::line-type line))))
    (if icon
        (with-room-for-graphics ()
          (draw-design stream icon))
        (format stream "~a" (string-downcase (cl-gopher::line-type line))))))

(define-presentation-type gopher-line () :inherit-from '((string)
                                                         :description "gopher line"))

(define-presentation-method present (gopher-line (type gopher-line) stream
                                             (view textual-view)
                                             &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a[gopher://~a:~a~a]"  (cl-gopher::display-string gopher-line)
          (cl-gopher::hostname gopher-line)
          (cl-gopher::port gopher-line)
          (cl-gopher::selector gopher-line)))

(define-presentation-method present (gopher-line (type gopher-line) stream
                                                 (view special-textual-view)
                                                 &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type gopher-line stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string gopher-line)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "gopher://~a:~a~a"
                (cl-gopher::hostname gopher-line)
                (cl-gopher::port gopher-line)
                (cl-gopher::selector gopher-line))))))

(define-presentation-type search () :inherit-from '((string)
                                                    :description "search"))

(define-presentation-method present (search (type search) stream
                                            (view textual-view)
                                            &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a gopher://~a:~a~a" (cl-gopher::display-string search)
          (cl-gopher::hostname search)
          (cl-gopher::port search)
          (cl-gopher::selector search)))

(define-presentation-method present (search (type search) stream
                                            (view special-textual-view)
                                            &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type search stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string search)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "gopher://~a:~a~a?~a"
                (cl-gopher::hostname search)
                (cl-gopher::port search)
                (cl-gopher::selector search)
                (cl-gopher::terms search))))))

(define-presentation-type info () :inherit-from '((string)
                                                  :description "info"))

(define-presentation-method present (info (type info) stream
                                          (view textual-view)
                                          &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream ""))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string info)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "")))))

(define-presentation-type html-file () :inherit-from '((string)
                                                  :description "html-file"))

(define-presentation-method present (html-file (type html-file) stream
                                          (view special-textual-view)
                                          &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (display-type html-file stream))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string html-file)))
  (with-application-frame (frame)
    (when (show-uri frame)
      (formatting-cell (stream :align-x :left)
        (format stream "~a" (cl-gopher::selector html-file))))))

(defun display-submenu (lines stream)
  (formatting-table (stream :x-spacing '(3 :character))
    (loop for line in lines
       do (formatting-row (stream)
            (case (cl-gopher::line-type line)
              (:info-message (present line 'info :stream stream))
              (:error-code   (present line 'info :stream stream))
              (:html-file    (present line 'html-file :stream stream
                                      :view (make-instance 'special-textual-view)))
;              (:html-file (formatting-cell (stream :align-x :left)
;                            (format stream "~a" line)))
;              (:submenu      (present line 'submenu :stream stream
;                                      :view (make-instance 'special-textual-view)))
;              (:text-file    (present line 'text-file :stream stream
;                                      :view (make-instance 'special-textual-view)))
              (:search       (present line 'search :stream stream
                                      :view (make-instance 'special-textual-view)))
;              (:image        (present line 'image :stream stream
;                                      :view (make-instance 'special-textual-view)))
;              (:gif          (present line 'image :stream stream
;                                      :view (make-instance 'special-textual-view)))
              (t             (present line 'gopher-line :stream stream
                                      :view (make-instance 'special-textual-view))))))))

(defun display-image (image stream)
  (let ((dl-name (format nil "/tmp/~a.~a" (pathname-name (cl-gopher::selector image))
                         (pathname-type (cl-gopher::selector image)))))
    (cl-gopher::download-file dl-name
                              (cl-gopher::hostname image)
                              (cl-gopher::port image)
                              (cl-gopher::selector image))

    (let* ((type (funcall (case (readtable-case *readtable*)
                            (:upcase #'string-upcase)
                            (:downcase #'string-downcase)
                            (t #'identity))
                          (pathname-type dl-name)))
           (format (find-symbol type (find-package :keyword))))
      (handler-case
          (with-application-frame (frame)
            (let ((pattern (make-pattern-from-bitmap-file dl-name :format format)))
              (with-room-for-graphics ()
                (draw-pattern* stream pattern 0 0))))
        (clim-extensions:unsupported-bitmap-format (e) nil)))))

(defun display-main (frame stream)
  (let ((current (car (history frame))))
    (case (cl-gopher::line-type current)
      (:submenu (let ((lines (cl-gopher::gopher-get-submenu current)))
                  (display-submenu lines stream)))
      (:search (let ((lines (cl-gopher::gopher-do-search current)))
                 (display-submenu lines stream)))
      (:text-file (let ((lines (cl-gopher::text-file-get-lines current)))
                    (loop for line in lines
                       do (format stream "~a~%" line))))
      (:image (display-image current stream))
      (:gif (display-image current stream))))

  (format *error-output* "Ending Redisplay.~%"))

(defun display-history (frame stream)
  (display-submenu (history frame) stream))

(defun make-button (title action)
  (make-pane 'push-button
             :label title
             :activate-callback (lambda (b)
                                  (declare (ignore b))
                                  (funcall action))))

(define-application-frame gopher ()
  ((history :initform nil :accessor history)
   (show-uri :initform nil :initarg :show-uri :accessor show-uri))
  (:panes
   (main-display
    :application
    :display-function 'display-main
    :display-time t
    :scroll-bars t)
   (history-display
    :application
    :display-function 'display-history
    :display-time t
    :scroll-bars t)
   (int :interactor))
  (:command-definer define-gopher-command)
  (:menu-bar t)
  (:layouts
   (default (vertically ()
              (99/100 main-display)
              (1/100 int)))
   (history (vertically ()
              (99/100
               (horizontally ()
                 (2/3 main-display)
                 (1/3 history-display)))
              (1/100 int)))))

;;; Redisplay machinery
(defclass main-redisplay-event (window-manager-event)
  ((item :initarg :item :accessor item)))

(defmethod handle-event ((frame gopher) (event main-redisplay-event))
  (format *error-output* "Handling redisplay event: ~a~%" event)
  (with-application-frame (frame)
    (let ((main-pane (find-pane-named frame 'main-display))
          (history-pane (find-pane-named frame 'history-display)))
      (setf (pane-needs-redisplay main-pane) t)
      (redisplay-frame-pane frame main-pane)

      (when history-pane
        (setf (pane-needs-redisplay history-pane) t)
        (redisplay-frame-pane frame history-pane)))))

(defun perform-main-redisplay (gopher-app)
  (format *error-output* "perform-main-redisplay~%")
  (queue-event (frame-top-level-sheet gopher-app)
               (make-instance 'main-redisplay-event
                              :sheet gopher-app)))


;;; Commands
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
      (default (setf (frame-current-layout frame) 'history))
      (history (setf (frame-current-layout frame) 'default)))
    (perform-main-redisplay frame)))

(define-gopher-command (com-toggle-uri-display :name t :menu t) ()
  (with-application-frame (frame)
    (setf (show-uri frame) (not (show-uri frame)))
    (perform-main-redisplay frame)))
    
(define-gopher-command (com-go-line :name t) ((gopher-line 'gopher-line))
  (with-application-frame (frame)
    (push gopher-line (history frame))
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator go-line
    (gopher-line com-go-line gopher
             :gesture :select		;command activated with left-click on a node
             :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-search :name t) ((search 'search))
  (with-application-frame (frame)
    (let ((mod-search (cl-gopher::copy-gopher-line search))
          (search-terms (clim:accept 'string :prompt "search")))
      (when (not (equal search-terms ""))
        (setf (cl-gopher::terms mod-search)
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
        (trivial-open-browser:open-browser (subseq (cl-gopher::selector html-file) 4))
      (uiop:subprocess-error (e) nil))))

(define-presentation-to-command-translator go-html
    (html-file com-go-html gopher
               :gesture :select		;command activated with left-click on a node
               :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-go-path :name t) ((path 'string))
  (let ((uri (if (and (>= (length path) 9) (equal "gopher://" (subseq path 0 9)))
                 (quri:uri path)
                 (quri:uri (format nil "gopher://~a" path)))))
    (with-application-frame (frame)
      (push
       (make-instance 'cl-gopher::gopher-line
                      :line-type :submenu
                      :display-string "???"
                      :selector (quri:uri-path uri)
                      :hostname (quri:uri-host uri)
                      :port (or (quri:uri-port uri) 70))
       (history frame))
      (perform-main-redisplay frame))))

(defvar *app*)
(defun browser ()
  (setf *app* (clim:make-application-frame 'clim-gopher::gopher))
  (setf (history *app*) (list (make-instance 'cl-gopher::gopher-line
                                             :line-type :submenu
                                             :display-string "Home"
                                             :selector "/"
                                             :hostname "gopher.floodgap.com"
                                             :port 70)))
  (clim:run-frame-top-level *app*))
