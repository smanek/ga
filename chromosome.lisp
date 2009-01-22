(in-package :ga)

(let ((uid 0))
  (defclass chromosome ()
    ((age      :accessor get-age ;;if all the chromosomes in my population are old, I've probably hit a local max
	       :initform 0)
     (uid      :initform (incf uid)
	       :reader uid))))