(defpackage :clim-gopher
  (:use :clim-lisp :clim)
  (:export browser))

(in-package :clim-gopher)
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *resource-path*
    (directory-namestring
     (merge-pathnames
      #P"res/"
      (load-time-value (or #.*compile-file-pathname* *load-pathname*))))))
