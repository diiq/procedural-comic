(in-package #:procedural-comic)

(defstruct rect
  top-left
  top-right
  bottom-left
  bottom-right)

; Lines are unique within a container
(defstruct line
  (start nil :type point)
  (end nil :type point))

(defstruct layout
  bounding-rect
  (h-lines ())
  (v-lines ())
  (d-lines ())
  (line-test ())
  (diagonalized-rects ()))

(defmethod panel-rect ((panel panel))
  (make-rect :top-left (panel-top-left panel)
             :top-right (panel-top-right panel)
             :bottom-left (panel-bottom-left panel)
             :bottom-right (panel-bottom-right panel)))

(defmethod layout-left-line ((layout layout))
  (make-line :start (rect-top-left (layout-bounding-rect layout))
             :end (rect-bottom-left (layout-bounding-rect layout))))

(defmethod layout-right-line ((layout layout))
  (make-line :start (rect-top-right (layout-bounding-rect layout))
             :end (rect-bottom-right (layout-bounding-rect layout))))

(defmethod layout-top-line ((layout layout))
  (make-line :start (rect-top-left (layout-bounding-rect layout))
             :end (rect-top-right (layout-bounding-rect layout))))

(defmethod layout-bottom-line ((layout layout))
  (make-line :start (rect-bottom-left (layout-bounding-rect layout))
             :end (rect-bottom-right (layout-bounding-rect layout))))

(defmethod maybe-add-line ((line line) (layout layout))
  (let ((points (cons-points (sort-points (line-start line) (line-end line)))))
    (if (member points (layout-line-test layout) :test #'equalp)
        ()
        (progn
          (layout-add-line-test points layout)
          line))))

(defmethod layout-add-h-line ((line line) (layout layout))
  (if (maybe-add-line line layout)
      (setf (layout-h-lines layout) (cons line (layout-h-lines layout)))))

(defmethod layout-add-v-line ((line line) (layout layout))
  (if (maybe-add-line line layout)
      (setf (layout-v-lines layout) (cons line (layout-v-lines layout)))))

(defmethod layout-add-d-line ((line line) (layout layout))
  (if (maybe-add-line line layout)
      (setf (layout-d-lines layout) (cons line (layout-d-lines layout)))))

(defmethod layout-add-diagonalized-rect (rect (layout layout))
  (let ((points (cons-points (sort-points (rect-top-left rect)
                                            (rect-top-right rect)
                                            (rect-bottom-left rect)
                                            (rect-bottom-right rect)))))
    (if (member points (layout-diagonalized-rects layout) :test #'equalp)
        ()
        (setf (layout-diagonalized-rects layout) (cons points (layout-diagonalized-rects layout))))))

(defmethod layout-add-line-test (points (layout layout))
  (setf (layout-line-test layout) (cons points (layout-line-test layout))))

(defun create-layout (rect)
  (let ((layout (make-layout :bounding-rect rect)))
    (layout-add-h-line (layout-top-line layout) layout)
    (layout-add-h-line (layout-bottom-line layout) layout)
    (layout-add-v-line (layout-left-line layout) layout)
    (layout-add-v-line (layout-right-line layout) layout)
    layout))

(defmethod layout-lines ((layout layout))
  (concatenate 'list
               (layout-h-lines layout)
               (layout-v-lines layout)
               (layout-d-lines layout)))

(defmethod pick-horizontal-line ((layout layout))
  (pick-one (layout-h-lines layout)))

(defmethod pick-vertical-line ((layout layout))
  (pick-one (layout-v-lines layout)))

(defmethod pick-diagonal-line ((layout layout))
  (pick-one (layout-d-lines layout)))

(defmethod pick-intersecting-lines ((layout layout))
  (let ((lines (pick-n 2 (layout-lines layout))))
    (if (and (apply #'line-intersection lines) (point-inside (apply #'line-intersection lines) layout))
        lines
        (pick-intersecting-lines layout))))

(defmethod line-intersection ((line-a line) (line-b line))
  (let* ((x1 (x (line-start line-a)))
         (x2 (x (line-end line-a)))
         (x3 (x (line-start line-b)))
         (x4 (x (line-end line-b)))
         (y1 (y (line-start line-a)))
         (y2 (y (line-end line-a)))
         (y3 (y (line-start line-b)))
         (y4 (y (line-end line-b)))
         (divisor (- (* (- x1 x2) (- y3 y4))
                     (* (- y1 y2) (- x3 x4)))))
    (unless (< (abs divisor) 0.01)
      (pt (/ (- (* (- (* x1 y2) (* y1 x2)) (- x3 x4))
                (* (- (* x3 y4) (* y3 x4)) (- x1 x2)))
             divisor)
          (/ (- (* (- (* x1 y2) (* y1 x2)) (- y3 y4))
                (* (- (* x3 y4) (* y3 x4)) (- y1 y2)))
             divisor)))))


;; Steps:
;; add horizontal
;; Add diagonal
;; add h, v, or diagonals until done

(defmethod uniqify-h-line ((line line) (layout layout))
  (make-line :start (line-intersection line (layout-left-line layout))
             :end (line-intersection line (layout-right-line layout))))

(defmethod uniqify-v-line ((line line) (layout layout))
  (make-line :start (line-intersection line (layout-top-line layout))
             :end (line-intersection line (layout-bottom-line layout))))

(defmethod point-inside ((m point) (layout layout))
  (let* ((top (layout-top-line layout))
         (left (layout-left-line layout))
         (b (line-start top))
         (a (line-end top))
         (c (line-end left))
         (ab (- b a))
         (am (- b m))
         (bc (- b c))
         (bm (- b m)))
    (and (< 0 (dot ab am) (dot ab ab))
         (< 0 (dot bc bm) (dot bc bc)))))

(defmethod create-diagonal ((rect rect))
  ;; Create a diagonal line across the rectangle, one direction or the other.
  (pick-one (list
             (make-line :start (rect-top-left rect) :end (rect-bottom-right rect))
             (make-line :start (rect-bottom-left rect) :end (rect-top-right rect)))))

(defun random-proportion ()
  (with-options-for 'proportion
    (almost-always 'proportion 1/3 2/3)
    (sometimes 'proportion 1/4 3/4 1/2)
    (+ (- 1/32 (* 1/16 (random 1.0))) (pick-a 'proportion))))

(defmethod create-random-horizontal ((rect rect))
  (let ((proportion (random-proportion))
        (left-span (- (rect-top-left rect) (rect-bottom-left rect)))
        (right-span (- (rect-top-right rect) (rect-bottom-right rect))))
    (make-line :start (+ (rect-bottom-left rect) (* left-span proportion))
               :end (+ (rect-bottom-right rect) (* right-span proportion)))))


(defmethod create-random-vertical ((rect rect))
  (let ((proportion (random-proportion))
        (top-span (- (rect-top-right rect) (rect-top-left rect)))
        (bottom-span (- (rect-bottom-right rect) (rect-bottom-left rect))))
    (make-line :start (+ (rect-top-left rect) (* top-span proportion))
               :end (+ (rect-bottom-left rect) (* bottom-span proportion)))))

(defmethod add-first-orthogonal ((layout layout))
  (if (equal (pick-one '(h v)) 'v)
      (layout-add-v-line (create-random-vertical (layout-bounding-rect layout)) layout)
      (layout-add-h-line (create-random-horizontal (layout-bounding-rect layout)) layout)))

(defmethod create-v-line-through ((pt point) (layout layout))
  (uniqify-v-line (make-line :start pt :end (+ (pt 0 1) pt)) layout))

(defmethod create-h-line-through ((pt point) (layout layout))
  (uniqify-h-line (make-line :start pt :end (+ (pt 1 0) pt)) layout))

(defmethod add-orthogonal ((layout layout))
  (let* ((lines (pick-intersecting-lines layout))
         (intersection (apply #'line-intersection lines)))
    (if (equal (pick-one '(h v)) 'v)
        (layout-add-v-line (create-v-line-through intersection layout) layout)
        (layout-add-h-line (create-h-line-through intersection layout) layout))))

(defmethod pick-rect ((layout layout))
  (let* ((hs (pick-n 2 (layout-h-lines layout)))
         (vs (pick-n 2 (layout-v-lines layout))))
    (make-rect :top-left (line-intersection (nth 0 hs) (nth 0 vs))
               :top-right (line-intersection (nth 0 hs) (nth 1 vs))
               :bottom-left (line-intersection (nth 1 hs) (nth 0 vs))
               :bottom-right (line-intersection (nth 1 hs) (nth 1 vs)))))

(defun cons-points (points)
  (map 'list (lambda (pt) (cons (round (x pt) 100) (round (y pt) 100))) points))


(defmethod add-diagonal ((layout layout))
  (let ((rect (pick-rect layout)))
    (if (layout-add-diagonalized-rect rect layout)
        (layout-add-d-line (create-diagonal rect) layout))))

(defmethod natural-grid (rect)
  (with-random-context
    (let ((layout (create-layout rect)))
      (add-diagonal layout)
      (add-first-orthogonal layout)
      (loop repeat (+ 5 (random 10))
         do (add-orthogonal layout)
         do (add-orthogonal layout)
         do (add-orthogonal layout)
         do (add-diagonal layout))
      layout)))

(defmethod draw-line (image (line line))
   (move-to image (line-start line))
   (line-to image (line-end line))
   (stroke-rgb image .9 .9 .9)
   (stroke image))

(defmethod draw-layout (image (layout layout))
  (loop for line in (layout-lines layout) do (draw-line image line)))



;; TODO: chose horizontal or vertical randomly but weighted based on proportion of panel
