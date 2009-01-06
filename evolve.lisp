(in-package :ga)

(defun make-population (goal &optional (size *population-size*))
  (loop for i from 1 upto size
       collect (make-random-chromosome goal)))

(defun solution (image-file)
  (let* ((goal (read-bmp-file image-file))
	 (population (sort (make-population goal) #'(lambda (a b) (< (get-score a) (get-score b)))))
	 (generation -1)
	 (best-score (get-score (car population))))
    (loop
       (incf generation)

       (when (< (get-score (car population)) best-score)
	 (write-chromosome (car population) generation (get-score (car population)))
	 (debug-print 3 "~%New overall best of ~A (previous best of ~A)~%~%" (get-score (car population)) best-score)
	 (setf best-score (get-score (car population))))

       (debug-print 1 "Generation ~A, and the best individual is ~A~%" generation (car population))

       (mapc #'(lambda (p) 
		 (incf (get-age p)))
	     population)

       (setf population (make-weighted-lst population #'get-score))
       (when (zerop (mod generation 100))
	 (debug-print 4 "~%Full population:~%~{~T~T~A~%~}" population))

       (setf population (safe-subseq
			 (sort (append (mapcar #'cdr population)
				       (mutate population *mutated-per-gen* goal)
				       (breed population *bred-per-gen* goal)
				       (make-population goal *new-per-gen*))
			       #'(lambda (a b) (< (get-score a) (get-score b))))
			 0 *population-size*)))))