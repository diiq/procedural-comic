(in-package #:procedural-comic)

(defclass string-image ()
  ((width :accessor width
          :initform 100
          :initarg :width)
   (height :accessor height
           :initform 100
           :initarg :height)
   (image  :accessor image
           :initform "
"
           :initarg :image)))

(defun string-image-add (image &rest strings)
  (setf (image image) (concatenate 'string (apply #'concatenate 'string (image image) strings) "
")))

(defmethod move-to ((image string-image) (point point))
  (string-image-add image (format nil "move-to~8,4T ~S" point)))

(defmethod line-to ((image string-image) (point point))
  (string-image-add image (format nil "line-to~8,4T ~S" point)))

(defmethod stroke ((image string-image))
  (string-image-add image "stroke"))
