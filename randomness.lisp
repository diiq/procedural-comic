(in-package #:procedural-comic)

(defvar *random-context-stack* nil)

(defmacro with-random-context (&body body)
    `(unwind-protect
       (progn (setf *random-context-stack* (cons () *random-context-stack*))
              ,@body)
       (setf *random-context-stack* (cdr *random-context-stack*))))

(defun set-chosen-value (name value)
  (if (equal *random-context-stack* nil) (error "No random context available. Try with-random-context."))
  (setf (car *random-context-stack*) (acons name value (car *random-context-stack*)))
  value)

(defun chosen-value (name)
  (labels ((chosen-value-iter (name context)
             (if (not context) (error "No value has been chosen for ~S in the current context" name))
             (if (assoc name (car context))
                 (cdr (assoc name (car context)))
                 (chosen-value-iter name (cdr context)))))
    (chosen-value-iter name *random-context-stack*)))



; Store distributions in a local env

(defvar *random-options* nil)

(defmacro with-options-for (name &body body)
    `(unwind-protect
       (progn (setf *random-options*
                    (acons ,name
                           (list (cons 'almost-always ()) (cons 'sometimes ()) (cons 'rarely ()))
                           *random-options*))
              ,@body)
       (setf *random-options* (cdr *random-options*))))

(defun options-for (name)
  (cdr (assoc name *random-options*)))

(defsetf options-for (name) (value)
  `(setf (cdr (assoc ,name *random-options*)) ,value))

(defun add-equal-option (option options)
  (let* ((count (length options))
         (multiplier (/ count (+ 1 count)))
         (single (/ 1 (+ 1 count))))
      (cons (cons single option) (loop for op in options collect (cons (* multiplier (car op)) (cdr op))))))

(defun equal-options (options)
  (let* ((count (length options))
         (single (if (> count 0) (/ 1 count) 1)))
      (loop for op in options collect (cons single op))))

(defun combine-option-lists (portion-a options-a portion-b options-b)
  (cond ((equal (length options-a) 0) options-b)
        ((equal (length options-b) 0) options-a)
        ('t (concatenate
             'list
             (loop for op in options-a collect (cons (* portion-a (car op)) (cdr op)))
             (loop for op in options-b collect (cons (* portion-b (car op)) (cdr op)))))))

(defun pick-from-options (options)
  (let ((num (random 1.0)))
    (labels ((iter (options sum)
      (if (> sum num)
          (cdar options)
          (iter (cdr options) (+ sum (caadr options))))))
    (iter options (caar options)))))

(defun set-rarity (rarity name value)
  (setf (cdr (assoc rarity (options-for name)))
        (cons value (cdr (assoc rarity (options-for name))))))

(defun sometimes (name &rest values)
  (mapc (lambda (x) (set-rarity 'sometimes name x)) values))

(defun almost-always (name &rest values)
  (mapc (lambda (x) (set-rarity 'almost-always name x)) values))

(defun rarely (name &rest values)
  (mapc (lambda (x) (set-rarity 'rarely name x)) values))

(defun pick-a (name)
  (let ((sometimes (cdr (assoc 'sometimes (options-for name))))
        (almost-always (cdr (assoc 'almost-always (options-for name))))
        (rarely (cdr (assoc 'rarely (options-for name)))))
    (set-chosen-value name (pick-from-options
                        (combine-option-lists
                         95/100 (equal-options almost-always)
                         5/100 (combine-option-lists 98/100 (equal-options sometimes)
                                                     2/100 (equal-options rarely)))))))

(defun pick-one (set)
  (nth (random (length set)) set))

(defsetf pick-one (set) (value)
  `(let ((n (random (length ,set))))
     (setf (nth n ,set) ,value)))

(defun remove-nth (n list)
  (append (subseq list 0 n) (nthcdr (+ 1 n) list)))

(defun pick-n (c set)
    (loop repeat c
       for mset = set then (remove-nth n mset)
       for n = (random (length mset))
       for it = (nth n mset)
       collecting it))
