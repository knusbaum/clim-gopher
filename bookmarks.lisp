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
                         :bookmarks (cl-gopher:unmarshall-gopher-lines gopher-lines))))
    (file-error (e) (make-instance 'bookmarks
                         :fname fname))))

(defun load-bookmarks ()
  (read-bookmarks
   (merge-pathnames "bookmarks.dat" *resource-path*)))

(defun bookmark-matches (gl1 gl2)
  (and
   (equalp (cl-gopher:hostname gl1) (cl-gopher:hostname gl2))
   (equalp (cl-gopher:port gl1) (cl-gopher:port gl2))
   (equalp (cl-gopher:selector gl1) (cl-gopher:selector gl2))))

(defun add-bookmark (bookmarks gl)
  (when (null (find gl (bookmarks bookmarks) :test #'bookmark-matches))
    (push gl (bookmarks bookmarks))
    (with-open-file (os (fname bookmarks) :direction :output :if-exists :supersede)
      (write (cl-gopher:marshall-gopher-lines (bookmarks bookmarks)) :stream os))))

(defun remove-bookmark (bookmarks gl)
  (setf (bookmarks bookmarks)
        (delete gl (bookmarks bookmarks) :test #'bookmark-matches))
  (with-open-file (os (fname bookmarks) :direction :output :if-exists :supersede)
    (write (cl-gopher:marshall-gopher-lines (bookmarks bookmarks)) :stream os)))
