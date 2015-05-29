(defpackage ch15)
(in-package ch15)

;;; Exercise 15.11
(defun sum (n1 n2)
  "Returns the sum of two nonnegative integers."
  (assert
   (and (integerp n1) (>= n1 0))
   (n1)
   "N1 must be a nonnegative integer, instead it is ~S." n1)
  (assert
   (integerp n2)
   (n2)
   "N2 must be an integer, instead it is ~S." n2)
  (cond ((zerop n1) n2)
	(t (sum (1- n1) (1+ n2)))))

;;; Exercise 15.12
(defun sum2 (n1 n2)
  "Returns the sum of two nonnegative integers."
  (assert
   (and (integerp n1) (>= n1 0))
   (n1)
   "N1 must be a nonnegative integer, instead it is ~S." n1)
  (assert
   (integerp n2)
   (n2)
   "N2 must be an integer, instead it is ~S." n2)
  (cond ((zerop n1) n2)
	(t (1+ (sum2 (1- n1) n2)))))

;;; Exercise 15.13
(defun product (n1 n2)
  "Returns the product of two nonnegative integers."
  (assert
   (and (integerp n1) (>= n1 0))
   (n1)
   "N1 must be a nonnegative integer, instead it is ~S." n1)
  (assert
   (and (integerp n2) (>= n2 0))
   (n2)
   "N2 must be a nonnegative integer, instead it is ~S." n2)
   (cond ((zerop n2) 0)
	 (t (sum n1 (product (1- n2) n1)))))

;;; Exercise 15.14
(defun power (n i)
  "For two nonnegative integers n, i, returns (expt n i)."
  (assert
   (and (integerp i) (>= i 0))
   (i)
   "I must be a positive integer; instead it is ~S." i)
  (cond ((zerop i) 1)
	(t (product n (power n (1- i))))))
  
