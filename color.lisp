(in-package :ga)

(defclass color ()
  ((red   :accessor get-red   
	  :initarg :red 
	  :initform 255
	  :type (unsigned-byte 8))
   (green :accessor get-green 
	  :initarg :green 
	  :initform 255
	  :type (unsigned-byte 8))
   (blue  :accessor get-blue  
	  :initarg :blue 
	  :initform 255
	  :type (unsigned-byte 8))
   (alpha :accessor get-alpha 
	  :initarg :alpha 
	  :initform 1
	  :type single-float)))

(defmethod print-object ((c color) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (red green blue alpha) c
      (format stream "(~A,~A,~A,~A)" red green blue alpha))))

(defparameter %red-byte-spec (byte 8 16))
(defparameter %green-byte-spec (byte 8 8))
(defparameter %blue-byte-spec (byte 8 0))

(defmethod color-to-rgb ((c color))
  (with-slots (red green blue alpha) c
    (the (unsigned-byte 24) (dpb (floor (/ red alpha)) %red-byte-spec ;;get rid of premultiplied alpha
				 (dpb (floor (/ green alpha)) %green-byte-spec
				      (dpb (floor (/ blue alpha)) %blue-byte-spec 0))))))

(defun rgb-to-color (c)
  (make-instance 'color :red (ldb %red-byte-spec c)
		        :green (ldb %green-byte-spec c)
			:blue (ldb %blue-byte-spec c)
			:alpha 1))

(defmethod combine-pixels ((original color) (new color))
  (with-slots ((ro red) (go green) (bo blue) (ao alpha)) original
    (with-slots ((rn red) (gn green) (bn blue) (an alpha)) new
      (make-instance 'color :red (+ rn (* (- 1 an) ro))
		            :green (+ gn (* (- 1 an) go))
			    :blue (+ bn (* (- 1 an) bn))
			    :alpha (+ an (* (- 1 an) ao))))))


(defmethod color-distance ((a color) (b color))
  (with-slots ((ro red) (go green) (bo blue)) a
    (with-slots ((rn red) (gn green) (bn blue)) b
      (+ (square (- ro rn))
	 (square (- go gn))
	 (square (- bo bn))))))

(defun make-random-color ()
  (let ((alpha (random 1.0)))
    (make-instance 'color :red (floor (* (random 256) alpha))
		          :green (floor (* (random 256) alpha))
			  :blue (floor (* (random 256) alpha))
			  :alpha alpha)))

(defmethod mutate-color ((c color))
  (let ((rand (random 1.0))
	(r (get-red c))
	(g (get-green c))
	(b (get-blue c))
	(a (get-alpha c)))
    (cond ((< rand 0.25) (setf r (* a (random 255))))
	  ((< rand 0.50) (setf g (* a (random 255))))
	  ((< rand 0.75) (setf b (* a (random 255))))
	  (t (setf r (/ r a)
		   g (/ g a)
		   b (/ b a)
		   a (random 1.0)
		   r (floor (* r a))
		   g (floor (* g a))
		   b (floor (* b a)))))
    (make-instance 'color :red r :green g :blue b :alpha a)))