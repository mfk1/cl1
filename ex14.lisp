;;; Exercise 14.2
(defun 14_2 (x y)
  "Returns 9.999.999.999 if y=0, else it returns x/y."
  (if (= y 0) 9999999999
    (/ x y)))

;;; Exercise 14.3
(defun absval (n)
  "Takes a number and returns its absolute value."
  (if (< n 0)
      (- n)
      n))

;;; Exercise 14.4
(defun sign (n)
  "Takes a number n and returns
- for n < 0
0 for n = 0
+ for n > 0"
  (if (< n 0)
      '-
      (if (= n 0)
	  0
	  '>)))

;;; Exercise 14.5
(defun sign1 (n)
  "Takes a number n and returns
- for n < 0
0 for n = 0
+ for n > 0"
  (cond ((< n 0) '-)
	((= n 0) 0)
	(t '+)))

;;; Exercise 14.6
(defun absval1 (n)
  "Takes a number and returns its absolute value."
  (cond ((< n 0) (- n))
	(t n)))
