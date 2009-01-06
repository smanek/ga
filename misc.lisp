(in-package :ga)

(defmacro debug-print (level control-string &rest format-arguments)
  `(when (> *debug-output* ,level)
     (format t (concatenate 'string "~tDEBUG:~t" ,control-string "~%") ,@format-arguments)))

(defun debug-level (level)
  (setf *debug-output* level))

(defun debug-stop ()
  (setf *debug-output* 0))

(defun debug-max ()
  (setf *debug-output* 10))

(defun square (x)
  (expt x 2))

(defun my-random (min max)
  (+ min (random (1+ (- max min)))))

(defun my-random-float (min max)
  (+ min (random (- max min))))

;;file i/o
(defun write-int16(int stream)
  (write-byte (ldb (byte 8 0) int) stream)
  (write-byte (ldb (byte 8 8) int) stream))

(defun write-int24(int stream)
  (write-byte (ldb (byte 8 0) int) stream)
  (write-byte (ldb (byte 8 8) int) stream)
  (write-byte (ldb (byte 8 16) int) stream))

(defun write-int32(int stream)
  (write-byte (ldb (byte 8 0) int) stream)
  (write-byte (ldb (byte 8 8) int) stream)
  (write-byte (ldb (byte 8 16) int) stream)
  (write-byte (ldb (byte 8 24) int) stream))

(defun read-int16 (stream)
  (let* ((a (read-byte stream))
	(b (read-byte stream)))
    (declare (type (unsigned-byte 8) a b))
    (the (unsigned-byte 16) (dpb a (byte 8 0)
				 (dpb b (byte 8 8) 0)))))

(defun read-int24 (stream)
  (let* ((a (read-byte stream))
	 (b (read-byte stream))
	 (c (read-byte stream)))
    (declare (type (unsigned-byte 8) a b c))
    (the (unsigned-byte 24) (dpb a (byte 8 0)
				 (dpb b (byte 8 8)
				      (dpb c (byte 8 16) 0))))))

(defun read-int32 (stream)
  (let* ((a (read-byte stream))
	 (b (read-byte stream))
	 (c (read-byte stream))
	 (d (read-byte stream)))
    (declare (type (unsigned-byte 8) a b c d))
    (the (unsigned-byte 32) (dpb a (byte 8 0)
				 (dpb b (byte 8 8)
				      (dpb c (byte 8 16)
					   (dpb d (byte 8 24) 0)))))))
(defmacro while (expression &body body)
  `(tagbody
    start (if (not ,expression) (go end))
      ,@body
      (go start)
    end))

(defun shuffle (sequence)
  "My own implementation of the Fisher-Yates shuffle"
  (let ((array-data (if (typep sequence 'vector)
			 (copy-seq sequence)
			 (coerce sequence 'vector)))
	 (len (length sequence))
	 (temp nil)
	 (rand nil))
    (while (> len 1)
      (setf rand (random len))
      (decf len)
      (setf temp (aref array-data len))
      (setf (aref array-data len) (aref array-data rand))
      (setf (aref array-data rand) temp))
    (coerce array-data (type-of sequence))))

(defun safe-subseq (lst start end)
  (subseq lst (if (and (< start (length lst))
		       (>= start 0))
		  start
		  0)
	  (if (and (<= end (length lst))
		   (>= end start))
	      end
	      (length lst))))

(defun insert-elt (lst elt position)
  (if (= position 0)
      (cons elt lst)
      (cons (car lst) (insert-elt (cdr lst) elt (1- position)))))

(defun remove-elt (lst position)
  (if (= position 0)
      (cdr lst)
      (cons (car lst) (remove-elt (cdr lst) (1- position)))))

(defun remove-random (lst)
  (remove-elt lst (random (length lst))))

(defun insert-random (lst item)
  (insert-elt lst item (random (length lst))))

(defun move-random (lst)
  (let* ((n (random (length lst)))
	 (e (elt lst n)))
    (insert-elt (remove-elt lst n) e (random (length lst)))))