(defpackage ch14)
(in-package ch14)

;;; EX 14.2

(defun div (x y)
  "Returns x/y if y != 0 and 9999999999 otherwise"
  (if (not (zerop y))
      (/ x y)
      9999999999))

;;; EX 14.3

(defun absval (x)
  (if (< x 0) (- x)
      x))

;;; EX 14.4 - sign (x) using if

(defun sign (x)
  (if (< x 0) '-
      (if (> x 0) '+
	  '0)))

;;; EX 14.5 sign (x) using cond

(defun sign1 (x)
  (cond ((< x 0) '-)
	((> x 0) '+)
	(t '0)))

;;; EX 14.6 absval (x) using cond

(defun absval1 (x)
  (cond ((< x 0) (- x))
	(t x)))
