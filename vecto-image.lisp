(in-package #:procedural-comic)

; Todo, make image automatically
(defclass vecto-image ()
  ((width :accessor width
          :initform 100
          :initarg :width)
   (height :accessor height
           :initform 100
           :initarg :height)
   (image  :accessor image
           :initarg :image)))

(defmethod initialize-instance :after ((image vecto-image) &rest args)
  (setf (image image)
        (make-vecto-image (width image) (height image))))

(defun make-vecto-image (width height)
  (let ((vecto-image nil))
    (vecto:with-canvas (:width width :height height)
      (setq vecto-image vecto::*graphics-state*))
    vecto-image))

(defmacro using-vecto-image (image &body body)
  `(let ((vecto::*graphics-state* (image ,image)))
     ,@body))

(defmethod move-to ((image vecto-image) (point point))
  (using-vecto-image image
    (vecto:move-to (x point) (y point))))

(defmethod line-to ((image vecto-image) (point point))
  (using-vecto-image image
    (vecto:line-to (x point) (y point))))

(defmethod circle ((image vecto-image) (center point) radius)
  (using-vecto-image image
    (vecto:arc (x center) (y center) radius 0 (* 2 pi))))

(defmethod stroke ((image vecto-image))
  (using-vecto-image image
    (vecto:stroke)))

(defmethod stroke-rgb ((image vecto-image) r g b)
  (using-vecto-image image
    (vecto:set-rgb-stroke r g b)))

(defmethod fill-rgb ((image vecto-image) r g b)
  (using-vecto-image image
    (vecto:set-rgb-fill r g b)
    (vecto:fill-path)))
