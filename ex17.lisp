(load "util")
(load "set")
(defpackage ch17
  (:shadow identity append reverse)
  (:import-from util element))
(in-package ch17)

;;; Exercise 17.1
(defun copy (l)
  "Returns the copy of a given list L."
  (check-type l list)
  (if (null l) '()
    (cons (first l) (copy (rest l)))))

;;; Exercise 17.3
(defun identity (object)
  "Returns its argument unmodified."
  object)

;;; Exercise 17.4
(defun append (l1 l2)
  "Takes two lists L1, L2 and returns a new list consisting of the members of L1 followed by the members of L2."
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
    (cons (first l1) (append (rest l1) l2))))

;;; Exercise17.5
(defun firstn (n l)
  "Returns a list whose members are the first n members of a given list L."
  (assert
   (and (integerp n) (<= n (length l)))
   (n)
   "N must be an integer >= (length l); instead it is ~S" n)
  (check-type l list)
  (if (zerop n) '()
    (cons (first l) (firstn (1- n) (rest l)))))

;;; Exercise 17.9
(defun reverse (l)
  "Returns a copy of a given list L with its members reversed."
  (check-type l list)
  (if (null l) '()
    (append (reverse (rest l)) (list (first l)))))

(defun reverse2 (l1 l2)
  "Takes two lists L1, L2; returns a list consisting of the members of L1 in reversed order, followed by L2."
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
    (reverse2 (rest l1) (cons (first l1) l2))))

(defun reverse1 (l)
  "Returns a copy of a given list L with its members reversed."
  (reverse2 l '()))

;;; Exercise 17.10
(defun sub-first (new old l)
  "Returns a copy of the list L with the element NEW replacing the first occurence of the element OLD."
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
	((eql (first l) old) (cons new (rest l)))
	(t (cons (first l) (sub-first new old (rest l))))))

;;; Exercise 17.12
(defun subst* (new old l)
  "Returns a copy of the list L with the element NEW replacing all occurences of the element OLD."
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
	((eql (first l) old) (cons new (subst* new old (rest l))))
	(t (cons (first l) (subst* new old (rest l))))))

;;;Exercise 17.28
(defun xprod (s1 s2)
  (check-type s1 (satisfies set::setp))
  (check-type s2 (satisfies set::setp))
  (cons :set (xprodh (rest s1) (rest s2))))

(defun xprodh (s1 s2)
  (cond ((null s1) '())
	(t (append (xprod1 (first s1) s2) (xprodh (rest s1) s2)))))

(defun xprod1 (e s)
  (cond ((null s) '())
	(t (cons (list e (first s)) (xprod1 e (rest s))))))

