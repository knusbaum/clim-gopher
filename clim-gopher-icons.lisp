(in-package :clim-gopher)

(defvar *icon-cache* (make-hash-table :test #'equal))

(defun get-type (file)
  (let ((mime (trivial-mimes:mime file)))
    (cond
      ((equal mime "image/gif") :gif)
      ((equal mime "image/png") :png)
      ((equal mime "image/jpg") :jpg)
      (t :bitmap))))

(defun get-icon (filename)
  (let ((dl-name (merge-pathnames *resource-path* filename)))
    (handler-case
        (let ((pattern (make-pattern-from-bitmap-file dl-name :format (get-type dl-name))))
          pattern)
      (clim-extensions:unsupported-bitmap-format (e) nil))))

(defun icon-for (type)
  (case type
    (:submenu (get-icon "folder.png"))
    (:search (get-icon "search.png"))
    (:text-file (get-icon "document.png"))
    (:binary-file (get-icon "binary.png"))
    (:gif (get-icon "image.png"))
    (:png (get-icon "image.png"))
    (:image (get-icon "image.png"))
    (:html-file (get-icon "html.png"))
    (:unknown (get-icon "unknown.png"))
    (t (get-icon "unknown.png"))))
