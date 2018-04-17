(in-package :clim-gopher)

(defvar *icon-cache* (make-hash-table :test #'equal))

(eval-when (:compile-toplevel)
  (defun get-type (file)
    (let ((mime (trivial-mimes:mime file)))
      (cond
        ((equal mime "image/gif") :gif)
        ((equal mime "image/png") :png)
        ((equal mime "image/jpg") :jpg)
        (t :bitmap))))

  (defun make-icon-array (filename)
    (let ((dl-name (merge-pathnames "res/" filename)))
      (format t "dl-name: ~a~%" dl-name)
      (handler-case
          (read-bitmap-file dl-name :format (get-type dl-name) :port nil)
        (clim-extensions:unsupported-bitmap-format (e) nil)))))

(defun make-icon-pattern (array)
  (make-instance 'clim-internals::rgb-pattern
                 :image (make-instance 'clim-internals::rgb-image
                                       :width (array-dimension array 1)
                                       :height (array-dimension array 0)
                                       :data array)))


(defun icon-for (type)
  (case type
    (:submenu (make-icon-pattern #.(make-icon-array "folder.png")))
    (:search-line (make-icon-pattern #.(make-icon-array "search.png")))
    (:text-file (make-icon-pattern #.(make-icon-array "document.png")))
    (:binary-file (make-icon-pattern #.(make-icon-array "binary.png")))
    (:gif (make-icon-pattern #.(make-icon-array "image.png")))
    (:png (make-icon-pattern #.(make-icon-array "image.png")))
    (:image (make-icon-pattern #.(make-icon-array "image.png")))
    (:html-file (make-icon-pattern #.(make-icon-array "html.png")))
    (:unknown (make-icon-pattern #.(make-icon-array "unknown.png")))
    (t (make-icon-pattern #.(make-icon-array "unknown.png")))))
