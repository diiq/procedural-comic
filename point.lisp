(in-package #:procedural-comic)


(defclass point ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)
   (weight :accessor weight
           :initarg :weight
           :initform nil)))

(defun pt (x y)
  (make-instance 'point :x x :y y))

(defmethod print-object ((point point) out)
  (format out "<~4D, ~4D>~@[ @ ~D~]" (x point) (y point) (weight point)))

(defmethod pt-length ((point point))
  (let ((x (x point))
        (y (y point)))
    (sqrt (+ (* x x) (* y y)))))


(defmethod * (x y)
  (cl:* x y))

(defmethod * ((point point) num)
  (pt (* (x point) num)
      (* (y point) num)))

(defmethod * (num (point point))
  (* point num))


(defmethod / (x y)
  (cl:/ x y))

(defmethod / ((point point) num)
  (pt (/ (x point) num)
      (/ (y point) num)))


(defgeneric + (x &rest xs))

(defmethod + (x &rest xs)
  (apply #'cl:+ x xs))

(defmethod + ((a point) &rest points)
  (pt (apply #'cl:+ (x a) (map 'list #'x points))
      (apply #'cl:+ (y a) (map 'list #'y points))))



(defgeneric - (x &rest xs))

(defmethod - (x &rest xs)
  (apply #'cl:- x xs))

(defmethod - ((a point) &rest points)
  (pt (apply #'cl:- (x a) (map 'list #'x points))
      (apply #'cl:- (y a) (map 'list #'y points))))

(defmethod flip-x ((a point))
  (pt (- (x a)) (y a)))

(defmethod flip-y ((a point))
  (pt (x a) (- (y a))))

;; Define equality?'

(defmethod dot ((a point) (b point))
  (+ (* (x a) (x b))
     (* (y a) (y b))))
