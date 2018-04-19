(defpackage :clim-gopher
  (:use :clim-lisp :clim)
  (:export browser))

(in-package :clim-gopher)
(eval-when (:load-toplevel :compile-toplevel)
  (defparameter *resource-path*
    (directory-namestring
     #+ccl (ccl:full-pathname (merge-pathnames
                           #P"res/"
                           #.*compile-file-pathname*))
     #-ccl (merge-pathnames
            #P"res/"
            #.*compile-file-pathname*))))
