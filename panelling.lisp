(in-package #:procedural-comic)


(defstruct panel
  top-left
  top-right
  bottom-left
  bottom-right
  borderless
  focal-point)

(defstruct circle-panel
  center
  radius)


(defun panelize (width height)
  (with-random-context
    (let ((panel (make-panel :bottom-left (pt 0 0)
                             :bottom-right (pt width 0)
                             :top-left (pt 0 height)
                             :top-right (pt width height))))
      (maybe-add-focal-points
       (shrink-panels
        (+ 5 (random 10))
        (maybe-set-one-borderless
         (mapcan (lambda (x) (maybe-add-inset-circle
                              (maybe-subdivide-one
                               (subdivide-horizontal-proportional (horizontal-divisions) x))))
                 (subdivide-vertical-proportional (vertical-divisions) (shrink-panel 5 panel)))))))))




(defun flatten (structure)
  (cond ((null structure) nil)
        ((atom structure) (list structure))
        (t (mapcan #'flatten structure))))

(defun subdivide-vertical-proportional (segments panel)
  (if (<= (length segments) 1)
      (list panel)
      (let ((total (reduce #'+ segments))
            (left-span (- (panel-top-left panel) (panel-bottom-left panel)))
            (right-span (- (panel-top-right panel) (panel-bottom-right panel))))
        (maybe-offset-point
         (loop for part in segments
            for sum = 0 then (+ sum portion)
            for portion = (/ part total)
            for new-panel = (make-panel
                             :bottom-left (+ (panel-bottom-left panel) (* left-span sum))
                             :top-left (+ (panel-bottom-left panel) (* left-span (+ sum portion)))
                             :bottom-right (+ (panel-bottom-right panel) (* right-span sum))
                             :top-right (+ (panel-bottom-right panel) (* right-span (+ sum portion))))
            collect new-panel)))))


(defun subdivide-horizontal-proportional (segments panel)
  (if (<= (length segments) 1)
      (list panel)
      (let ((total (reduce #'+ segments))
            (bottom-span (- (panel-bottom-right panel) (panel-bottom-left panel)))
            (top-span (- (panel-top-right panel) (panel-top-left panel))))
        (loop for part in segments
           for sum = 0 then (+ sum portion)
           for portion = (/ (ceiling (* 10 (/ part total))) 10)
           for end = (min 1 (+ sum portion))
           for new-panel = (make-panel
                            :bottom-left (+ (panel-bottom-left panel) (* bottom-span sum))
                            :bottom-right (+ (panel-bottom-left panel) (* bottom-span end))
                            :top-left (+ (panel-top-left panel) (* top-span sum))
                            :top-right (+ (panel-top-left panel) (* top-span end)))
           collect new-panel))))

(defmethod shrink-panel (gutter (panel panel))
  (let ((partial-gutter (pt (/ gutter 2) gutter)))
    (make-panel :top-left (+ (panel-top-left panel) (flip-y partial-gutter))
                :top-right (+ (panel-top-right panel) (* -1 partial-gutter))
                :bottom-left (+ (panel-bottom-left panel) partial-gutter)
                :bottom-right (+ (panel-bottom-right panel) (flip-x partial-gutter))
                :borderless (panel-borderless panel))))

(defmethod shrink-panel (gutter (panel circle-panel))
  (make-circle-panel
   :center (circle-panel-center panel)
   :radius (- (circle-panel-radius panel) gutter)))

(defun shrink-panels (gutter panelset)
  (mapcar (lambda (p) (shrink-panel gutter p)) panelset))

(defun maybe-set-one-borderless (panelset)
  (with-options-for 'borderless
    (almost-always 'borderless nil)
    (sometimes 'borderless 't)
    (if (equal (chosen-value 'vertical-divisions) 1)
        (almost-always 'borderless 't))
    (if (pick-a 'borderless)
        (setf (panel-borderless (nth (random (length panelset)) panelset)) 't)))
  panelset)

(defun maybe-offset-point (panelset)
  (with-options-for 'offset
    (almost-always 'offset nil)
    (cond ((equal (chosen-value 'vertical-divisions) 2)
           (sometimes 'offset 't))
          ((> (chosen-value 'vertical-divisions) 3)
           (rarely 'offset 't)))
    (if (pick-a 'offset)
        (let* ((panel (car panelset))
               (old-point (panel-top-left panel))
               (left-span (- old-point (panel-bottom-left panel)))
               (new-point (+ old-point (* left-span (/ (pick-one '(-1 1)) (+ 5 (random 3)))))))
          (setf (panel-top-left panel) new-point)
          (setf (panel-bottom-left (nth 1 panelset)) new-point))))
  panelset)

(defun maybe-subdivide-one (panelset)
  (with-options-for 'subdivide
    (almost-always 'subdivide nil)
    (if (> (chosen-value 'horizontal-divisions) 2)
        (sometimes 'subdivide 't))
    (if (pick-a 'subdivide)
        (let ((n (random (length panelset))))
          (setf (nth n panelset) (subdivide-vertical (random 3) (nth n panelset)))
          (flatten panelset))
        panelset)))

(defmethod add-focal-point ((panel panel))
  (with-options-for 'focal-point
    (with-options-for 'focal-point-horizontal
      (with-options-for 'focal-point-vertical
        (almost-always 'focal-point 't)
        (sometimes 'focal-point 'nil)

        (sometimes 'focal-point-horizontal 1/3 2/3 1/2)
        (rarely 'focal-point-horizontal (random 1.0))

        (sometimes 'focal-point-vertical 2/3 2/3 2/3 1/2 1/2 1/2 1/3)
        (rarely 'focal-point-vertical (random 1.0))

        (if (pick-a 'focal-point)
            (let* ((vertical (pick-a 'focal-point-vertical))
                   (horizontal (pick-a 'focal-point-horizontal))
                   (left-point (+ (panel-bottom-left panel)
                                  (* vertical (- (panel-top-left panel)
                                                 (panel-bottom-left panel)))))
                   (right-point (+ (panel-bottom-right panel)
                                   (* vertical (- (panel-top-right panel)
                                                  (panel-bottom-right panel)))))
                   (point (+ left-point (* horizontal (- right-point left-point)))))
              (setf (panel-focal-point panel) point))))))
  panel)


(defmethod add-focal-point ((panel circle-panel))
  panel)

(defun maybe-add-focal-points (panelset)
  (mapcar #'add-focal-point panelset))


(defun maybe-add-inset-circle (panelset)
  (with-options-for 'add-circle
    (almost-always 'add-circle nil)
    (if (and (> (length panelset) 1) (< (length panelset) 4))
        (rarely 'add-circle 't nil nil))
    (if (and (> (length panelset) 1) (pick-a 'add-circle))
        (let* ((panel (nth (random (- (length panelset) 1)) panelset))
               (height (- (panel-top-right panel) (panel-bottom-right panel)))
               (width (- (panel-top-right panel) (panel-top-left panel)))
               (smallest (min (pt-length height) (pt-length width)))
               (center (+ (panel-bottom-right panel) (/ height 2))))
          (concatenate 'list panelset (list (make-circle-panel :center center :radius (/ smallest 4)))))
        panelset)))

(defun divisions (count)
  (loop for x from 1 to count collect (+ 5 (division-size))))

(defun division-size ()
  (with-options-for 'division-size
    (sometimes 'division-size 5 6 7 8 9 10)
    (pick-a 'division-size)))

(defun horizontal-divisions ()
  (with-options-for 'horizontal-divisions
    (sometimes 'horizontal-divisions 1 2 3)
    (rarely 'horizontal-divisions 4 5)
    (if (equal (chosen-value 'vertical-divisions) 1)
        (almost-always 'horizontal-divisions 1))

    (if (equal (chosen-value 'vertical-divisions) 2)
        (progn (almost-always 'horizontal-divisions 1 2 3)))
    (divisions (pick-a 'horizontal-divisions))))

(defun vertical-divisions ()
  (with-options-for 'vertical-divisions
    (sometimes 'vertical-divisions 1)
    (almost-always 'vertical-divisions 2 3 3)
    (rarely 'vertical-divisions 4 5)
    (divisions (pick-a 'vertical-divisions))))

(defmethod draw-panel (image (panel panel))
  (unless (panel-borderless panel)
    (move-to image (panel-top-left panel))
    (line-to image (panel-top-right panel))
    (line-to image (panel-bottom-right panel))
    (line-to image (panel-bottom-left panel))
    (line-to image (panel-top-left panel))
    (stroke image))
  (if (panel-focal-point panel)
      (draw-focal-point image (panel-focal-point panel))))

(defun draw-focal-point (image point)
  (move-to image (- point (pt 5 0)))
  (line-to image (+ point (pt 5 0)))
  (stroke image)
  (move-to image (- point (pt 0 5)))
  (line-to image (+ point (pt 0 5)))
  (stroke image))

(defmethod draw-panel (image (panel circle-panel))
  (circle image (circle-panel-center panel) (circle-panel-radius panel))
  (fill-rgb image 1 1 1)
  (circle image (circle-panel-center panel) (circle-panel-radius panel))
  (stroke image))

(defun draw-panelset (image panelset)
  (mapc (lambda (x) (draw-panel image x)) panelset))
