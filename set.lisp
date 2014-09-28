(defpackage set
	    (:shadow set))
(in-package :set)

(defun setp (l)
  "Returns T if there are no two elements of L which are EQL"
  (if (not (listp l)) nil
  (cond ((null l) t)
	((member (first l) (rest l)) nil)
	(t (setp (rest l))))))
