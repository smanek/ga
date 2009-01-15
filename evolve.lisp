(in-package :ga)

(defun make-population (goal weighted-old-population count &key (random 0) (breed 0) (mutate 0))
  "This function will create the 'next generation' of a population."
  
  ;;ensure I'm not trying to breed/mutate a non-existent population
  (assert (or (not (null weighted-old-population))
	      (and (zerop breed)
		   (zerop mutate))))

  (let ((threads nil))  
    ;;break the creation of new chromosomes off into new threads, and get the results later, as possible
    (push (sb-thread:make-thread (lambda () (mutate weighted-old-population mutate goal))) threads)
    (push (sb-thread:make-thread (lambda () (breed weighted-old-population breed goal))) threads)
    (push (sb-thread:make-thread (lambda () (make-random-chromosomes goal random))) threads)

    (safe-subseq (sort (append (mapcar #'cdr weighted-old-population)
			       (mapcan #'sb-thread:join-thread threads))
		       #'(lambda (a b) (< (get-score a) (get-score b))))
		 0 count)))

(defun solution (image-file)
  "Image file should be the full path to a bitmap image you wish to run this program on."
  (let* ((goal (read-bmp-file image-file))
	 (population (make-population goal nil *population-size* :random *population-size*))
	 (generation -1)
	 (best-score (1+ (get-score (car population)))))

    (loop
       (incf generation)

       ;;If the best member of my current population is better than the previous best, write it out to a file
       (when (< (get-score (car population)) best-score)
	 (write-chromosome (car population) generation (get-score (car population)))
	 (debug-print 3 "~%New overall best of ~A (previous best of ~A)~%~%" (get-score (car population)) best-score)
	 (setf best-score (get-score (car population))))

       (debug-print 1 "Generation ~A, and the best individual is ~A~%" generation (car population))

       ;;age all the chromosomes. If all the chromosomes are old, it suggests that the GA is stuck on a local maxima
       (mapc #'(lambda (p) 
		 (incf (get-age p)))
	     population)

       (setf population (make-weighted-lst population #'get-score))
       
       ;;output the full population every thousand generations, if debugging is enabled
       (when (zerop (mod generation 1000))
	 (debug-print 4 "~%Full population:~%~{~T~T~A~%~}" population))
       
       ;;create the next generation
       (setf population (make-population goal population *population-size* :random *new-per-gen* 
									   :breed *bred-per-gen* 
									   :mutate *mutated-per-gen*)))))