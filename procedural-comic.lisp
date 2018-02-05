;;;; procedural-comic.lisp

(in-package #:procedural-comic)

(defun page (image width height)
  (draw-panelset image (panelize width height)))

(defun png-page (width height filename)
  (let ((image (make-instance 'vecto-image :width width :height height)))
    (page image width height)
    (using-vecto-image image (vecto:save-png filename))))

(defun string-page (width height)
  (let ((image (make-instance 'string-image :width width :height height)))
    (page image width height)
    (image image)))

(defun comic (w h pages location)
  (loop for i from 1 to pages
       do (png-page w h (format nil "~A/page_~D.png" location i))))

; call: (comic 768 1024 24 "~/comic/") ; to generate a 24 page comic layout
