;;;; procedural-comic.lisp

(in-package #:procedural-comic)

(defun picture (image)
  (move-to image (make-instance 'point :x 0 :y 0))
  (line-to image (make-instance 'point :x 100 :y 100))
  (stroke image)
  image)

(print (image (picture (make-instance 'string-image))))
(using-vecto-image (picture (make-instance 'vecto-image :image (make-vecto-image 300 300)))
  (vecto:save-png "~/ok.png"))


(setq p1 (make-instance 'point :x 100 :y 100))
(* p1 5)
(* 5 p1)
