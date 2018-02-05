;;;; procedural-comic.lisp

(in-package #:procedural-comic)

(defun picture (image)
  (move-to image (make-instance 'point :x 0 :y 0))
  (line-to image (make-instance 'point :x 100 :y 100))
  (stroke image)
  image)

(defun white-out-page (image width height)
    (move-to image (pt 0 0))
    (line-to image (pt width 0))
    (line-to image (pt width height))
    (line-to image (pt 0 height))
    (line-to image (pt 0 0))
    (fill-rgb image 1 1 1))

(defun png-page (width height filename)
  (let ((image (make-instance 'vecto-image :width width :height height)))
    (white-out-page image width height)
    (draw-panelset image (panelize width height))
    (using-vecto-image image (vecto:save-png filename))))

(defun string-page (width height)
  (let ((image (make-instance 'string-image :width width :height height)))
    (white-out-page image width height)
    (draw-panelset image (panelize width height))
    (image image)))

(defun comic (w h pages)
  (loop for i from 1 to pages
       do (png-page w h (format nil "~~/comic/page_~D.png" i))))


(string-page 100 300)

(comic 768 1024 100)
