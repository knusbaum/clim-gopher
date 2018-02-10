(in-package :clim-gopher)

(defclass special-textual-view (textual-view) ())

(define-presentation-type submenu () :inherit-from '((string)
                                                     :description "submenu"))

(define-presentation-method present (submenu (type submenu) stream
                                                 (view textual-view)
                                                 &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a gopher://~a:~a~a"  (cl-gopher::display-string submenu)
          (cl-gopher::hostname submenu)
          (cl-gopher::port submenu)
          (cl-gopher::selector submenu)))

(define-presentation-method present (submenu (type submenu) stream
                                                 (view special-textual-view)
                                                 &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream "dir"))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string submenu)))
  (formatting-cell (stream :align-x :left)
    (format stream (format nil "gopher://~a:~a~a"
                           (cl-gopher::hostname submenu)
                           (cl-gopher::port submenu)
                           (cl-gopher::selector submenu)))))

(define-presentation-type text-file () :inherit-from '((string)
                                                     :description "text-file"))

(define-presentation-method present (text-file (type text-file) stream
                                               (view textual-view)
                                               &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a gopher://~a:~a~a" (cl-gopher::display-string text-file)
          (cl-gopher::hostname text-file)
          (cl-gopher::port text-file)
          (cl-gopher::selector text-file)))

(define-presentation-method present (text-file (type text-file) stream
                                               (view special-textual-view)
                                               &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream "file"))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string text-file)))
  (formatting-cell (stream :align-x :left)
    (format stream "gopher://~a:~a~a"
            (cl-gopher::hostname text-file)
            (cl-gopher::port text-file)
            (cl-gopher::selector text-file))))

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
    (format stream "Search"))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string search)))
  (formatting-cell (stream :align-x :left)
    (format stream "gopher://~a:~a~a"
            (cl-gopher::hostname search)
            (cl-gopher::port search)
            (cl-gopher::selector search))))

(define-presentation-type image () :inherit-from '((string)
                                                     :description "image"))

(define-presentation-method present (image (type image) stream
                                               (view textual-view)
                                               &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a gopher://~a:~a~a" (cl-gopher::display-string image)
          (cl-gopher::hostname image)
          (cl-gopher::port image)
          (cl-gopher::selector image)))

(define-presentation-method present (image (type image) stream
                                               (view special-textual-view)
                                               &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream "Image"))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" (cl-gopher::display-string image)))
  (formatting-cell (stream :align-x :left)
    (format stream "gopher://~a:~a~a"
            (cl-gopher::hostname image)
            (cl-gopher::port image)
            (cl-gopher::selector image))))

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
  (formatting-cell (stream :align-x :left)
    (format stream "")))

(defun display-submenu (lines stream)
  (formatting-table (stream :x-spacing '(3 :character))
    (loop for line in lines
       do (formatting-row (stream)
            (case (cl-gopher::line-type line)
              (:info-message (present line 'info :stream stream))
              (:error-code (present line 'info :stream stream))
              (:submenu (present line 'submenu :stream stream
                                 :view (make-instance 'special-textual-view)))
              (:text-file (present line 'text-file :stream stream
                                   :view (make-instance 'special-textual-view)))
              (:search (present line 'search :stream stream
                                :view (make-instance 'special-textual-view)))
              (:image (present line 'image :stream stream
                               :view (make-instance 'special-textual-view)))
              (:gif (present line 'image :stream stream
                             :view (make-instance 'special-textual-view)))
              (t (progn
                   (formatting-cell (stream :align-x :left)
                     (format stream "~a" (cl-gopher::line-type line)))
                   (formatting-cell (stream :align-x :left)
                     (format stream (cl-gopher::display-string line)))
                   (formatting-cell (stream :align-x :left)
                     (format stream "gopher://~a:~a~a"
                             (cl-gopher::hostname line)
                             (cl-gopher::port line)
                             (cl-gopher::selector line))))))))))
  
(defun display-main (frame stream)
  (let ((lines (curr-lines frame))
        (file (display-file frame))
        (img (display-image frame)))

    (cond
      (img 
       (with-room-for-graphics ()
         (draw-pattern* *standard-output* img 0 0)))
      (file
        (loop for line in file
           do (format stream "~a~%" line)))
      (lines
       (display-submenu lines stream))))
  
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
   (curr-lines :initform nil :accessor curr-lines)
   (display-file :initform nil :accessor display-file)
   (display-image :initform nil :accessor display-image))
  (:panes
   (main-display
    :application
    :display-function 'display-main
    :display-time t)
   (history-display
    :application
    :display-function 'display-history
    :display-time t)
   (int :interactor))
  (:command-definer define-gopher-command)
  (:command-table (gopher
                   :inherit-from ()
                   :menu (("Back"       :command "Back")
                          ("Refresh"    :command "Refresh")
                          ("History"    :command "History"))))
  (:menu-bar t)
  (:layouts
   (default (vertically ()
              (99/100 main-display)
              (1/100 int)))
   (history (vertically ()
              (99/100
               (horizontally ()
                 (1/2 history-display)
                 (1/2 main-display)))
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
;(defun back-button ()
(define-gopher-command (com-back :name t :menu t) ()
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

    (perform-main-redisplay frame)))

(define-gopher-command (com-refresh :name t :menu t) ()
  (with-application-frame (frame)
    (let ((curr (car (history frame))))
      (setf (curr-lines frame)
            (cl-gopher:gopher-get-directory
             (cl-gopher::hostname curr)
             (cl-gopher::port curr)
             (cl-gopher::selector curr)))
      (perform-main-redisplay frame))))

(define-gopher-command (com-history :name t :menu t) ()
  (with-application-frame (frame)

    (case (frame-current-layout frame)
      (default (setf (frame-current-layout frame) 'history))
      (history (setf (frame-current-layout frame) 'default)))
    
    (perform-main-redisplay frame)))

;(define-gopher-command (com-history :name t :menu t) ()
;  (with-application-frame (frame)
;    (cond
;      ((display-image frame)
;       (setf (display-image frame) nil))
;
;      ((display-file frame)
;       (setf (display-file frame) nil)))
;
;    (setf (curr-lines frame)
;          (history frame))
;
;    ;; (push nil (history frame))
;
;    (perform-main-redisplay frame)))

(define-gopher-command (com-go-submenu :name t) ((submenu 'submenu))
  (with-application-frame (frame)
    (push submenu (history frame))
    (setf (curr-lines frame)
          (cl-gopher:gopher-get-directory
           (cl-gopher::hostname submenu)
           (cl-gopher::port submenu)
           (cl-gopher::selector submenu)))
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator go-submenu
    (submenu com-go-submenu gopher
      :gesture :select		;command activated with left-click on a node
      :menu t)              ;includes this command in right-click menu
    (object) (list object))

;(define-gopher-command (com-bookmark :name t) ((submenu 'submenu))
;  (with-application-frame (frame)
;    (

(define-gopher-command (com-read-text-file :name t) ((text-file 'text-file))
  (with-application-frame (frame)
    (setf (display-file frame)
          (cl-gopher:get-text-file-lines
           (cl-gopher::hostname text-file)
           (cl-gopher::port text-file)
           (cl-gopher::selector text-file)))
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator read-text-file
    (text-file com-read-text-file gopher
      :gesture :select		;command activated with left-click on a node
      :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-search :name t) ((search 'search))
  (with-application-frame (frame)
    (push search (history frame))
    (setf (curr-lines frame)
          (cl-gopher:gopher-get-directory
           (cl-gopher::hostname search)
           (cl-gopher::port search)
           (format nil "~a~a~a" (cl-gopher::selector search) #\tab (clim:accept 'string :prompt "search"))))
    (perform-main-redisplay frame)))

(define-presentation-to-command-translator search
    (search com-search gopher
      :gesture :select		;command activated with left-click on a node
      :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-display-image :name t) ((image 'image))
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
              (setf (display-image frame) pattern)
              (perform-main-redisplay frame)))
        (clim-extensions:unsupported-bitmap-format (e) nil)))))

(define-presentation-to-command-translator image
    (image com-display-image gopher
      :gesture :select		;command activated with left-click on a node
      :menu t)              ;includes this command in right-click menu
    (object) (list object))

(define-gopher-command (com-go-path :name t) ((path 'string))
  (let ((uri (if (and (>= (length path) 9) (equal "gopher://" (subseq path 0 9)))
                 (quri:uri path)
                 (quri:uri (format nil "gopher://~a" path)))))
    (with-application-frame (frame)
      (setf (curr-lines frame)
            (cl-gopher:gopher-get-directory
             (quri:uri-host uri)
             (or (quri:uri-port uri) 70)
             (quri:uri-path uri)))
      (perform-main-redisplay frame))))

(defvar *app*)
(defun browser ()
  (setf *app* (clim:make-application-frame 'clim-gopher::gopher))
  (setf (history *app*) (list (make-instance 'cl-gopher::gopher-line
                                             :line-type :submenu
                                             :display-string "SDF"
                                             :selector "/"
                                             :hostname "sdf.org"
                                             :port 70)))
  (setf (curr-lines *app*) (cl-gopher:gopher-get-directory "sdf.org" 70 "/"))
  (clim:run-frame-top-level *app*))
