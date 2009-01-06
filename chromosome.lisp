(in-package :ga)

(defclass chromosome ()
  ((polygons :accessor get-polygons 
	     :initarg :polygons)
   (image    :accessor get-image
	     :initarg :image)
   (score    :accessor get-score
	     :initarg :score)
   (width    :initarg :width
	     :accessor get-width)
   (height   :initarg :height
	     :accessor get-height)
   (source   :reader get-source
	     :initarg :source
	     :initform 'default)
   (age      :accessor get-age
	     :initform 0)
   (uid      :initarg :uid
	     :initform 0
	     :reader uid)))

(defmethod print-object ((c chromosome) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (score source polygons age uid) c
      (format stream "~A: Score = ~A, Source = ~A, polygons = ~A, age = ~A" uid score source (length polygons) age))))

(defmethod make-image ((c chromosome))
  (let ((image (make-bitmap (get-width c) (get-height c))))
    (loop for polygon in (get-polygons c)
       do (draw-filled-polygon image polygon)
       finally (return image))))


(let ((uid 0))
  (defun make-chromosome (polygons goal-bmp source)
    (destructuring-bind (x y) (array-dimensions goal-bmp)
      (let ((res (make-instance 'chromosome 
				:polygons polygons
				:width x
				:height y
				:source source
				:uid (incf uid))))     
	(setf (get-image res) (make-image res))
					;      (setf (get-image res) (make-bitmap x y))
	(setf (get-score res) (difference-bmps (get-image res) goal-bmp x y))
	(debug-print 9 "Creating a chromosome (~A, ~A) with:~%~{~T~T~A~%~}~%" source (get-score res) polygons)
	res))))

(defun make-random-chromosome (goal-bmp)
  (destructuring-bind (x y) (array-dimensions goal-bmp)
    (make-chromosome (make-random-polygons *min-initial-polygons* *max-initial-polygons* 
					   x y
					   *max-polygon-vertices*)
		     goal-bmp
		     'random)))

(defmethod offspring ((a chromosome) (b chromosome) goal)
  (let ((res (make-chromosome (loop for polygon in (get-polygons a) 
				 with other-index = 0
				 with other-len = (length (get-polygons b))
				 with other-p = (shuffle (get-polygons b))
				 collecting (if (and (> (random 1.0) *crossover-rate*)
						     (> other-len other-index))
						(prog1 
						    (elt other-p other-index)
						  (incf other-index))
						polygon))
			      goal
			      'breeding)))
    (debug-print 7 "Creating child chromosome ~A from ~A and ~A." res a b)
    res))

(defmethod mutate-chromosome ((a chromosome) goal)
  (let ((res (make-chromosome (mutate-polygons (get-polygons a) (get-width a) (get-height a)) 
			      goal 'mutation)))
    (debug-print 7 "Mutating chromosome ~A to ~A" a res)
    res))

(defun mutate (wpop count goal)
  (loop for i from 1 upto count 
     collecting (mutate-chromosome (car (choose-from-weighted-lst wpop 1)) goal)))

(defmethod write-chromosome ((a chromosome) generation number)
  (write-bmp-file (get-image a)  
		  (format nil "~A~A-~A~A" *output-directory* generation number *output-type*)))

(defun breed (wpop children goal)
  (loop for i from 1 upto children collecting (apply #'offspring 
						     (append (choose-from-weighted-lst wpop 2)
							     (list goal)))))