(in-package :ga)

(defun make-weighted-lst (lst metric &optional (key #'identity) (lower-better t))
  "Assumes lower score is better. Set :lower-better nil to reverse this assumption"
  ;;an example of a weighted list might be: ((.1 . 'a) (.5 . 'b) (1.0 . 'c)).
  ;;in that list, there is a 10% chance of choosing 'a. A 40% chance of choosing 'b. And a 50% chance of choosing 'c
  (if lower-better
      (make-weighted-lst lst (lambda (e) 
			       (/ 1 (funcall metric e))) key nil)
      (let ((sum-scores (apply #'+ (mapcar metric lst)))
	    (running-total 0))
	(mapcar #'(lambda (c) 
		    (cons (incf running-total (/ (funcall metric c) sum-scores)) 
			  (funcall key c)))
		lst))))

(defun update-weighted-lst (wlst)
  "Updates the scores in a weighted list with one or more elements removed"
  (make-weighted-lst (loop for e in wlst
			with prev = 0
			collecting (prog1 
				       (cons (- (car e) prev) (cdr e))
				     (setf prev (car e))))
		     #'car
		     #'cdr
		     nil))

(defun %choose-single-from-weighted-lst (weighted-list)
  "Choose a single element from a weighted list. An aux. fn for choose-from-weighted-lst  "
  (loop for i from 0 upto (1- (length weighted-list))
     with rand = (random 1.0)
     when (> (car (elt weighted-list i)) rand)
     do (return (values (cdr (elt weighted-list i)) i))))

(defun choose-from-weighted-lst (wlst n)
  "Choose n distinct elements from a weighted list wlst. Weighted lists should be created with make-weighted-lst"
  (let ((res nil))
    (dotimes (i n res)      
      (multiple-value-bind (value index) 
	  (%choose-single-from-weighted-lst wlst)
	(setf res (cons value res))
	(setf wlst (update-weighted-lst (remove-elt wlst index)))))))

(defun choose-from-weighted-lst-with-repeats (wlst n)
  "Choose n elements from a weighted list wlst. Weighted lists should be created with make-weighted-lst"
  (loop for i from 1 upto n collecting (%choose-single-from-weighted-lst wlst)))