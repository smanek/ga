(in-package :ga)

(defclass thread-job ()
  ((fn    :initarg :fn
          :initform (error "You must supply a function for me to compute")
	  :reader fn)
   (count :initarg :eval-count
	  :initform 1
	  :reader eval-count)))

(defun threading-support-p ()
  (when (member :sb-thread *features*)
      t))

(defmethod eval-thread-job ((tj thread-job))
  (if (threading-support-p) ;;detect if the current lisp implementation supports SBCL style threading
      (loop for i from 1 upto (eval-count tj)
	 collecting (sb-thread:make-thread (fn tj)) into threads ;;if so, use it
	 finally (return (mapcar #'sb-thread:join-thread threads)))
      (loop for i from 1 upto (eval-count tj) ;;otherwise, just process the lambdas sequentially
	 collecting (funcall (fn tj)))))

(defmethod eval-thread-job ((tj function))
  ;;just a convenience method, so you don't have to instantiate a thread-job object for a lambda that only needs to run once
  (funcall tj))

(defun eval-thread-jobs (&rest tjs)
  (if (threading-support-p) ;;detect if the current lisp implementation supports SBCL style threading
      (loop for tj in tjs 
	 collecting (sb-thread:make-thread (lambda () (eval-thread-job tj))) into threads ;;if it does, process the tjs in parrallel
	 finally (return (mapcan #'sb-thread:join-thread threads)))
      (mapcan #'eval-thread-job tjs))) ;;otherwise, process them sequentially