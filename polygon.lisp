(in-package :ga)

(defclass polygon ()
  ;;a simple polygon class that consists of three or more vertices, and a color
  ((vertices    :accessor get-vertices    
		:initarg :vertices
		:type cons)
   (color       :accessor get-color
		:initarg :color
		:type color))) 

(defmethod print-object ((poly polygon) stream)
  (print-unreadable-object (poly stream :type t)
    (with-slots (vertices color) poly
      (format stream "Vertices: ~A, Color: ~A" vertices color))))

(defun make-random-vertices (max-x max-y max-vertices)
  (loop for i from 1 upto (my-random 3 max-vertices)
     collecting (list (random max-x) (random max-y))))

(defun make-random-polygon (max-x max-y max-vertices)
  (make-instance 'polygon :vertices (make-random-vertices max-x max-y max-vertices)
		          :color (make-random-color)))

(defmethod mutate-polygon ((p polygon) max-x max-y)
  ;;mutate a polygon, in accordance with the probabilities in global.lisp
  (let ((v (get-vertices p))
	(x-shift 0)
	(y-shift 0)
	(scale 1)
	(must-loop nil)
	(c (get-color p)))
    (when (and (< (random 1.0) *remove-vertex-rate*)
	       (> (length v) 3))
      (setf v (remove-random v)))
    (when (< (random 1.0) *insert-vertex-rate*)
      (setf v (insert-random v (list (random max-x) (random max-y)))))
    (when (< (random 1.0) *poly-shift-rate*)
      (setf x-shift (random max-x))
      (setf y-shift (random max-y))
      (setf must-loop t))
    (when (< (random 1.0) *poly-scale-rate*)
      (setf scale (random 2.0))
      (setf must-loop t))
    (when must-loop
      (setf v (loop for vertex in v 
		 collecting (list (mod (floor (* scale (+ (car vertex) x-shift))) max-x)
				  (mod (floor (* scale (+ (cadr vertex) y-shift))) max-y)))))
    (when (< (random 1.0) *color-mutate-rate*)
      (setf c (mutate-color c)))
    (make-instance 'polygon :vertices v :color c)))

(defun mutate-polygons (polygons max-width max-height)
  "Mutate a list of polygons, in accordance to the probabilities in global.lisp"
  (let ((p (loop for polygon in polygons collecting
		(if (< (random 1.0) *mutation-rate*)
		    (mutate-polygon polygon max-width max-height)
		    polygon))))
    (when (and  (< *min-polygons* (length polygons))
		(< (random 1.0) *deletion-rate*))
      (setf p (remove-random polygons)))
    (when (and (< (length polygons) *max-polygons*) 
	       (< (random 1.0) *insertion-rate*))
      (setf p (insert-random p (make-random-polygon max-width max-height *max-polygon-vertices*))))

    (when (and (> 1 (length p))
	       (< (random 1.0) *shuffle-rate*))
      (setf p (move-random p)))
    p))

(defun make-random-polygons (min-polygons max-polygons max-x max-y max-vertices)
  (loop for i from 1 upto (my-random min-polygons max-polygons) 
     collecting (make-random-polygon max-x max-y max-vertices)))

(defun draw-line (bmp start end color)
  "A straightforward implementation of Bresenham's Algorithm to draw a line on a bitmap"
  (destructuring-bind ((x0 y0) (x1 y1)) (list start end)
    (declare (type fixnum x0 y0 x1 y1))
    (let ((steep (> (abs (- y1 y0)) (abs (- x1 x0)))))
      (when steep
	(rotatef x0 y0)
	(rotatef x1 y1))
      (when (> x0 x1)
	(rotatef x0 x1)
	(rotatef y0 y1))
      (let* ((deltax (- x1 x0))
	     (deltay (abs (- y1 y0)))
	     (err (/ deltax 2))
	     (ystep (if (< y0 y1)
			1
			-1))
	     (y y0))
	(loop for x from x0 upto x1
	   if steep do (setf (get-pixel-bmp bmp y x) color)
	   else do  (setf (get-pixel-bmp bmp x y) color)
	   do (decf err deltay)
	   when (< err 0)
	   do (setf y (+ y ystep)
		    err (+ err deltax)))))))

(defclass edge ()
  ;;an intermediate class, just used to pass data around. Just a convenience
  ((x0 :accessor get-x0 :initarg :x0)
   (y0 :accessor get-y0 :initarg :y0)
   (x1 :accessor get-x1 :initarg :x1)
   (y1 :accessor get-y1 :initarg :y1)))

(defmethod print-object ((e edge) stream)
  (print-unreadable-object (e stream :type t)
    (with-slots (x0 y0 x1 y1) e
      (format stream "(~A, ~A) => (~A,~A)" x0 y0 x1 y1))))

(defmethod get-edges ((polygon polygon))
  ;;convert the vertices of a polygon to a series of edges
  (let ((last-v (last (get-vertices polygon))))
    (cons (make-instance 'edge :x0 (caar last-v) 
			       :y0 (cadar last-v)
			       :x1 (caar (get-vertices polygon))
			       :y1 (cadar (get-vertices polygon)))
	  (get-edges-aux (get-vertices polygon)))))

(defun get-edges-aux (vertices)
  (when (not (null (cdr vertices)))
    (destructuring-bind ((x0 y0) (x1 y1) &rest r) vertices
      (declare (ignore r))
      (cons (make-instance 'edge :x0 x0 :y0 y0 :x1 x1 :y1 y1) (get-edges-aux (cdr vertices))))))

(defmethod draw-edge (bmp (e edge) color)
  ;;draw an edge on a bitmap
  (with-slots (x0 y0 x1 y1) e
    (draw-line bmp (list x0 y0) (list x1 y1) color)))

(defmethod draw-unfilled-polygon (bmp (polygon polygon))
  ;;to draw an unfille polygon, we just need to draw its edges
  (mapc #'(lambda (edge) (draw-edge bmp edge (get-color polygon))) (get-edges polygon)))

(defmethod %edge-table-entry ((e edge))
  ;;an auxillary function, to help efficiently draw filled algorithms.
  ;;the edge list is a list of edges, with four edges per edge. 
  ;;The minimum y, the maximum y, the x corresponding to the minimum y, and 1/m.
  (with-slots (x0 y0 x1 y1) e
    (when (< y1 y0)
      (rotatef x0 x1)
      (rotatef y0 y1))
    (let ((dy (- y1 y0))
	  (dx (- x1 x0)))
      (when (not (zerop dy))
	(list y0 y1 x0 (/ dx dy))))))

(defmethod draw-filled-polygon (bmp (polygon polygon))
  "Uses the even-odd rule for complex polygons to draw a filled polygon"
  ;;I basically send scanlines down the bitmap, but it's important to keep track of the edge table so this technique is efficient
  (let* ((unsorted  (remove-if #'null (mapcar #'%edge-table-entry (get-edges polygon))))
	 (edge-list  (when unsorted (sort unsorted
					  #'(lambda (a b) (cond ((< (car a) (car b)) t)
								((> (car a) (car b)) nil)
								(t (< (third a) (third b)))))))))
    (when edge-list 
      (%draw-filled-polygon-aux bmp edge-list nil (1- (caar edge-list)) (get-color polygon)))))

(defun %draw-filled-polygon-aux (bmp global-edge-list active-edge-list scan-line-y color)
  "Actually run the scan lines, using the edge table that was initialized by draw-filled-polygons"
  (destructuring-bind (max-x max-y) (array-dimensions bmp)
    (when (and (< scan-line-y max-y) (or global-edge-list active-edge-list))
      (loop for edge in global-edge-list while (= (car edge) scan-line-y)
	 do (progn  
	      (setf active-edge-list (append active-edge-list (list (cdr edge))))
	      (setf global-edge-list (cdr global-edge-list)))) ;;move edges to the active list
      (loop for edge in active-edge-list while (= (car edge) scan-line-y)
	 do (setf active-edge-list (cdr active-edge-list))) ;;remove stale edges from the active list
      (loop for edge in active-edge-list
	 do (setf (second edge) (+ (second edge) (third edge))) ;;update x-vals for active edges, and resort on x-val
	 finally (setf active-edge-list (sort active-edge-list #'(lambda (a b) (< (second a) (second b))))))
      (when active-edge-list
	(loop for x from 0 upto (1- max-x)
	   with active-index = 0
	   with parity = nil
	   with len-active = (length active-edge-list)
	   when (and (< active-index len-active) 
		     (= x (floor  (second (elt active-edge-list active-index)))))
	   do (progn
		(setf parity (not parity))
		(incf active-index))
	   and when (and (< active-index len-active)  ;;seems kind of silly, but I need to have it check this condition twice, for some vertices
		     (= x (floor  (second (elt active-edge-list active-index)))))
	   do (progn
		(setf parity (not parity))
		(incf active-index))
	   when parity do (setf (get-pixel-bmp bmp x scan-line-y) color)))
      (%draw-filled-polygon-aux bmp global-edge-list active-edge-list (1+ scan-line-y) color))))