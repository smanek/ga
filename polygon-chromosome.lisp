(in-package :ga)

(defclass polygon-chromosome (chromosome)
  ;;I keep the image/score around because they are very expensive to compute
  ;;and I want to avoid redoing work I've already done
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
   (source   :reader get-source  ;;source is useful for debugging. It tells me if a chromosome is a child, mutation, etc.
	     :initarg :source
	     :initform 'default)))

(defmethod print-object ((c polygon-chromosome) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (score source polygons age uid) c
      (format stream "~A: Score = ~A, Source = ~A, polygons = ~A, age = ~A" uid score source (length polygons) age))))

(defmethod make-image ((c polygon-chromosome))
  ;;compute the image for polygon-chromosome, by drawing its polygons on its bitmap
  (let ((image (make-bitmap (get-width c) (get-height c))))
    (loop for polygon in (get-polygons c)
       do (draw-filled-polygon image polygon)
       finally (return image))))

  (defun make-polygon-chromosome (polygons goal-bmp source)
    "I want to create an image and compute a score anytime I create a polygon-chromosome. This constructor fn seemed cleaner than :after methods."
    (destructuring-bind (x y) (array-dimensions goal-bmp)
      (let ((res (make-instance 'polygon-chromosome 
				:polygons polygons
				:width x
				:height y
				:source source)))     
	(setf (get-image res) (make-image res))
					;      (setf (get-image res) (make-bitmap x y))
	(setf (get-score res) (difference-bmps (get-image res) goal-bmp x y))
	(debug-print 9 "Creating a polygon-chromosome (~A, ~A) with:~%~{~T~T~A~%~}~%" source (get-score res) polygons)
	res)))

(defun make-random-polygon-chromosome (goal-bmp)
  "Make a random chromsome of the same size as goal-bmp (and whose score is computed as its distance from the goal)"
  (destructuring-bind (x y) (array-dimensions goal-bmp)
    (make-polygon-chromosome (make-random-polygons *min-initial-polygons* *max-initial-polygons* 
					   x y
					   *max-polygon-vertices*)
		     goal-bmp
		     'random)))

(defun make-random-polygon-chromosomes (goal-bmp count)
  "Make count random polygon-chromosomes"
  (eval-thread-job (make-instance 'thread-job :fn (lambda () (make-random-polygon-chromosome goal-bmp))
				              :eval-count count)))

(defmethod offspring-polygon-chromosomes ((a polygon-chromosome) (b polygon-chromosome) goal)
  "Create the offspring of two chromsomes. I found I got better results if I didn't mutate here, 
and instead seperately mutated members of my population."
  (let ((res (make-polygon-chromosome (loop for polygon in (get-polygons a) 
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
    (debug-print 7 "Creating child polygon-chromosome ~A from ~A and ~A." res a b)
    res))

(defmethod mutate-polygon-chromosome ((a polygon-chromosome) goal)
  "Mutate a polygon-chromosome in accordance to the probabilities in global.lisp."
  (let ((res (make-polygon-chromosome (mutate-polygons (get-polygons a) (get-width a) (get-height a)) 
			      goal 'mutation)))
    (debug-print 7 "Mutating polygon-chromosome ~A to ~A" a res)
    res))

(defun mutate-polygon-chromosomes (wpop count goal)
  "Mutate count individuals from a weighted list of a population"
  (eval-thread-job (make-instance 'thread-job :fn (lambda () (mutate-polygon-chromosome (car (choose-from-weighted-lst wpop 1)) goal))
				              :eval-count count)))

(defmethod write-polygon-chromosome ((a polygon-chromosome))
  (write-bmp-file (get-image a)  
		  (format nil "~A~A-~A~A" *output-directory* (uid a) (get-score a) *output-type*)))

(defun breed-polygon-chromosomes (wpop children goal)
  "Create children from a weighted list of a population"
  (eval-thread-job (make-instance 'thread-job :fn (lambda () (apply #'offspring-polygon-chromosomes
								    (append (choose-from-weighted-lst wpop 2)
									    (list goal))))
				              :eval-count children)))