(in-package :clim-gopher)

(defclass bookmarks ()
  ((bookmarks :initarg :bookmarks :initform nil :accessor bookmarks)))

(defmethod make-load-form ((self bookmarks) &optional environment)
  (make-load-form-saving-slots self
                               :slot-names '(bookmarks)
                               :environment environment))

(defmethod make-load-form ((self cl-gopher:gopher-line) &optional environment)
  (make-load-form-saving-slots self
                               :slot-names '(cl-gopher:display-string cl-gopher:selector cl-gopher:hostname cl-gopher:port)
                               :environment environment))

(defun read-bookmarks (fname)
  (handler-case
      (with-open-file (is fname)
        (let ((*read-eval* nil))
          (let ((gopher-lines (read is)))
            (make-instance 'bookmarks
                           :bookmarks (cl-gopher:gopher-lines-from-alist gopher-lines)))))
    (file-error (e)
      (declare (ignore e))
      nil)))

(defvar *default-bookmarks*
  '(((:port . 70)
     (:hostname . "gopherpedia.com")
     (:selector . "/lookup")
     (:display-string . "Search Gopherpedia")
     (:line-type . :search-line))
    ((:port . 70)
     (:hostname . "gopherpedia.com")
     (:selector . "")
     (:display-string . "Gopherpedia, the gopher interface to Wikipedia")
     (:line-type . :submenu))
    ((:port . 70)
     (:hostname . "gopher.floodgap.com")
     (:selector . "/fun/xkcd")
     (:display-string
      . "xkcd - A Webcomic of Romance, Sarcasm, Math and Language.")
     (:line-type . :submenu))
    ((:port . 70)
     (:hostname . "hngopher.com")
     (:selector . "")
     (:display-string . "HN Gopher - A Hacker News Mirror")
     (:line-type . :submenu))
    ((:port . 70)
     (:hostname . "gopher.floodgap.com")
     (:selector . "/v2/vs")
     (:display-string . "Search Veronica-2")
     (:line-type . :search-line))
    ((:port . 70)
     (:hostname . "gopher.floodgap.com")
     (:selector . "/v2")
     (:display-string . "Search Gopherspace with Veronica-2")
     (:line-type . :submenu))))

(defun bookmarks-path ()
  (merge-pathnames ".gopher-bookmarks.dat" (user-homedir-pathname)))

(defun load-bookmarks ()
  (or
   (read-bookmarks (bookmarks-path))
   (make-instance 'bookmarks
                  :bookmarks (cl-gopher:gopher-lines-from-alist *default-bookmarks*))))

(defun bookmark-matches (gl1 gl2)
  (and
   (equalp (cl-gopher:hostname gl1) (cl-gopher:hostname gl2))
   (equalp (cl-gopher:port gl1) (cl-gopher:port gl2))
   (equalp (cl-gopher:selector gl1) (cl-gopher:selector gl2))))

(defun add-bookmark (bookmarks gl)
  (when (null (find gl (bookmarks bookmarks) :test #'bookmark-matches))
    (push gl (bookmarks bookmarks))
    (with-open-file (os (bookmarks-path) :direction :output :if-exists :supersede)
      (write (cl-gopher:gopher-lines-to-alist (bookmarks bookmarks)) :stream os))))

(defun remove-bookmark (bookmarks gl)
  (setf (bookmarks bookmarks)
        (delete gl (bookmarks bookmarks) :test #'bookmark-matches))
  (with-open-file (os (bookmarks-path) :direction :output :if-exists :supersede)
    (write (cl-gopher:gopher-lines-to-alist (bookmarks bookmarks)) :stream os)))
