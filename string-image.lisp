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

(defmethod circle ((image string-image) (center point) radius)
  (string-image-add image (format nil "circle ~S ~D" center radius)))

(defmethod fill-rgb ((image string-image) r g b)
  (string-image-add image (format nil "fill-rgb ~2F ~2F ~2F" r g b)))
