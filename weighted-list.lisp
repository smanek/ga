(in-package :ga)

(defun make-weighted-lst (lst metric &optional (key #'identity) (lower-better t))
  "Assumes lower score is better"
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
  (make-weighted-lst (loop for e in wlst
			with prev = 0
			collecting (prog1 
				       (cons (- (car e) prev) (cdr e))
				     (setf prev (car e))))
		     #'car
		     #'cdr
		     nil))

(defun %choose-single-from-weighted-lst (weighted-list)
  (loop for i from 0 upto (1- (length weighted-list))
     with rand = (random 1.0)
     when (> (car (elt weighted-list i)) rand)
     do (return (values (cdr (elt weighted-list i)) i))))

(defun choose-from-weighted-lst (wlst n)
  (let ((res nil))
    (dotimes (i n res)      
      (multiple-value-bind (value index) 
	  (%choose-single-from-weighted-lst wlst)
	(setf res (cons value res))
	(setf wlst (update-weighted-lst (remove-elt wlst index)))))))

(defun choose-from-weighted-lst-with-repeats (wlst n)
  (loop for i from 1 upto n collecting (%choose-single-from-weighted-lst wlst)))