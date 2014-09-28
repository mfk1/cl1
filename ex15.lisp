(defpackage ch15)
(in-package ch15)
;;; Recursions

;;; EX 15.2

(defun sum (n1 n2)
  "Returns the sum of two nonnegative integers"
  (assert (and (integerp n1) (>= n1 0))
	  (n1)
	  "N1 must be an integer with N1 >= 0; instead it's ~S" n1)
  (assert (integerp n2)
	  (n2)
	  "N2 must be an integer; instead it's ~S" n2)
  (if (zerop n1) n2
      (sum (1- n1) (1+ n2))))

;;; EX 15.12

(defun sum2 (n1 n2)
  "Returns the sum of two nonnegtive integers"
  (assert (and (integerp n1) (>= n1 0))
	  (n1)
	  "N1 must be a nonnegative integer; instead it's ~S" n1)
  (assert (integerp n2)
	  (n2)
	  "N1 must be an integer; instead it's ~S" n2)
  (if (zerop n1) n2
      (1+ (sum (1- n1) n2))))

;;; EX 15.13

(defun product (n1 n2)
  "Returns the product of two nonnegative integers"
  (assert (and (integerp n1) (>= n1 0))
	  (n1)
	  "N1 must be a nonnegative integer; instead it's ~S" n1)
  (assert (integerp n2)
	  (n2)
	  "N2 must be an integer; instead it's ~S")
  (if (zerop n1) 0
      (sum2 n2 (product (1- n1) n2))))
;;; EX 15.14

(defun power (n i)
  (if (zerop i) 1
      (product n (power n (1- i)))))
