(in-package :clim-gopher)

(defvar *icon-cache* (make-hash-table :test #'equal))

(defun get-icon (filename)
  (let ((dl-name (merge-pathnames *resource-path* filename)))
    (let* ((type (funcall (case (readtable-case *readtable*)
                            (:upcase #'string-upcase)
                            (:downcase #'string-downcase)
                            (t #'identity))
                          (pathname-type dl-name)))
           (format (find-symbol type (find-package :keyword))))
      (handler-case
          (let ((pattern (make-pattern-from-bitmap-file dl-name :format format)))
            pattern)
        (clim-extensions:unsupported-bitmap-format (e) nil)))))

(defun icon-for (type)
  (case type
    (:submenu (get-icon "folder.png"))
    (:search (get-icon "search.png"))
    (:text-file (get-icon "document.png"))
    (:binary-file (get-icon "binary.png"))
    (:gif (get-icon "image.png"))
    (:png (get-icon "image.png"))
    (:image (get-icon "image.png"))
    (:html-file (get-icon "html.png"))))
