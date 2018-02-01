(in-package #:procedural-comic)


(defclass point ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)
   (weight :accessor weight
           :initarg :weight
           :initform nil)))

(defmethod print-object ((point point) out)
  (format out "<~4D, ~4D>~@[ @ ~D~]" (x point) (y point) (weight point)))

(defmethod length ((point point))
  (let ((x (x point))
        (y (y point)))
    (sqrt (* x x) (* y y))))

(shadow '*)

(defmethod * (x y)
  (cl:* x y))

(defmethod * ((point point) num)
  (make-instance 'point :x (* (x point) num) :y (* (y point) num)))

(defmethod * (num (point point))
  (* point num))


(shadow '/)

(defmethod / (x y)
  (cl:/ x y))

(defmethod / ((point point) num)
  (make-instance 'point :x (/ (x point) num) :y (/ (y point) num)))


(shadow '+)

(defgeneric + (x &rest xs))

(defmethod + (x &rest xs)
  (apply #'cl:+ x xs))

(defmethod + ((a point) &rest points)
  (make-instance 'point
                 :x (apply #'cl:+ (x a) (map 'list #'x points))
                 :y (apply #'cl:+ (y a) (map 'list #'y points))))


(shadow '-)

(defgeneric - (x &rest xs))

(defmethod - (x &rest xs)
  (apply #'cl:- x xs))

(defmethod - ((a point) &rest points)
  (make-instance 'point
                 :x (apply #'cl:- (x a) (map 'list #'x points))
                 :y (apply #'cl:- (y a) (map 'list #'y points))))


; Define equality?'
