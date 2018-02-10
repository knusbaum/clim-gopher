(defpackage :clim-error-reproduction
  (:use :clim-lisp :clim)
  (:export test))

(in-package :clim-error-reproduction)

(defclass special-textual-view (textual-view) ())


(define-presentation-type element () :inherit-from '((string)
                                                     :description "element"))

(define-presentation-method present (element (type element) stream
                                             (view special-textual-view)
                                             &key acceptably)
  (declare (ignore acceptably))
  (formatting-cell (stream :align-x :left)
    (format stream "~a" element)))

(define-presentation-method present (element (type element) stream
                                             (view textual-view)
                                             &key acceptably)
  (declare (ignore acceptably))
  (format stream "~a" element))

(defun display-main (frame stream)
  (let ((lines (curr-lines frame)))
    (when lines
      (formatting-table (stream :x-spacing '(3 :character))
        (loop for line in lines             
           do (formatting-row (stream)
                (present line 'element :stream stream :view (make-instance 'special-textual-view))))))))

(define-application-frame error-app ()
  ((curr-lines :initform nil :accessor curr-lines))
  (:panes
   (main-display
    :application
    :display-function 'display-main
    :display-time t)
   (int :interactor))
  (:command-definer define-some-command)
  (:layouts
   (default (vertically ()
              (99/100 main-display)
              (1/100 int)))))

(define-some-command (com-go-element :name t) ((element 'element))
  t)

(defvar *app*)
(defun test ()
  (setf *app* (clim:make-application-frame 'error-app))
  (setf (curr-lines *app*) '("Line 1" "Line 2"))
  (clim:run-frame-top-level *app*))
