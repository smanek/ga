(in-package :ga)

(defun abstract-make-population (weighted-old-population create-ex-nihlio mutate-wpop breed-wpop fitness-metric 
				  total-count new-count breed-count mutate-count)

  (safe-subseq (sort (append (mapcar #'cdr weighted-old-population)
			     (eval-thread-jobs (lambda () (funcall mutate-wpop weighted-old-population mutate-count))
					       (lambda () (funcall breed-wpop weighted-old-population breed-count))
					       (lambda () (funcall create-ex-nihlio new-count))))
		     #'(lambda (a b) (< (funcall fitness-metric a) (funcall fitness-metric b))))
	       0 total-count))

(defun abstract-evolution (create-ex-nihlio mutate-wpop breed-wpop fitness-metric save-chromosome
			   total-count new-count breed-count mutate-count)
  (let* ((population (abstract-make-population nil create-ex-nihlio mutate-wpop breed-wpop fitness-metric total-count total-count 0 0))
	 (best-score (1+ (funcall fitness-metric (car population))))
	 (generation 0))
    (loop
       (incf generation)
       
       ;;If the best member of my current population is better than the previous best, write it out to a file
       (when (< (funcall fitness-metric (car population)) best-score)
	 (funcall save-chromosome (car population))
	 (debug-print 3 "~%New overall best of ~A (previous best of ~A)~%~%" (funcall fitness-metric (car population)) best-score)
	 (setf best-score (funcall fitness-metric (car population))))

       (debug-print 1 "Generation ~A, and the best individual is ~A~%" generation (car population))
       
       ;;age all the chromosomes. If all the chromosomes are old, it suggests that the GA is stuck on a local maxima
       (mapc #'(lambda (p) 
		 (incf (get-age p)))
	     population)

       (setf population (make-weighted-lst population fitness-metric))
       
       ;;output the full population every thousand generations, if debugging is sufficiently high
       (when (zerop (mod generation 1000))
	 (debug-print 4 "~%Full population:~%~{~T~T~A~%~}" population))
       
       ;;create the next generation
       (setf population (abstract-make-population population create-ex-nihlio mutate-wpop breed-wpop fitness-metric
					 total-count new-count breed-count mutate-count)))))

(defun polygon-evolution (image-file)
  (let ((goal (read-bmp-file image-file)))
    (abstract-evolution #'(lambda (count)
			    (make-random-polygon-chromosomes goal count))
			#'(lambda (wpop count)
			    (mutate-polygon-chromosomes wpop count goal))
			#'(lambda (wpop count)
			    (breed-polygon-chromosomes wpop count goal))
			#'get-score
			#'write-polygon-chromosome
			*population-size*
			*new-per-gen*
			*bred-per-gen*
			*mutated-per-gen*)))



