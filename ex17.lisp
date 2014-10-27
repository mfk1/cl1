(load "util.lisp")
(defpackage ch17
  (:shadow copy identity append reverse)
  (:import-from util element))
(in-package :ch17)

;;; Exercise 17.01
(defun copy (l)
  "Returns a copy of the list L"
  (check-type l list)
  (if (null l) '()
    (cons (first l) (copy (rest l)))))

;;; Exercise 17.03
(defun identity (obj)
  "Returns its argument unmodified"
  obj)

;;; Exercise 17.04
(defun append (l1 l2)
  "Returns a list consisting of the members of l1 followed by the members of l2"
  (assert (listp l1) (l1)
	  "L1 must be a list, instead it's ~S" l1)
  (assert (listp l2) (l2)
	  "L2 must be a list, instead it's ~s" l2)
  (if (null l1) l2
    (cons (first l1) (append (rest l1) l2))))

;;;Exercise 17.05
(defun firstn (n l)
  "Returns a list consisting of the first n members of the given list L"
  (check-type n integer)
  (assert (and (listp l) (>= (length l) n)) (l)
	  "L must be a list which must be at least n members long" l)
  (if (zerop n) '()
    (cons (first l) (firstn (1- n) (rest l)))))

;;;Exercise 17.09 part 1
(defun reverse (l)
  "Returns a copy of the list L1 with the order of its members reversed"
  (check-type l list)
  (if (null l) '()
    (append (reverse (rest l)) (list (first l)))))

;;; Exercise 17.09 part 2
(defun reverse2 (l1 l2)
  "Returns a list consisting of the members of L1 in reversed order followed by the members of L2 in original order"
  (check-type l1 list)
  (check-type l2 list)
  (if (null l1) l2
    (reverse2 (rest l1) (cons (first l1) l2))))

(defun reverse1 (l)
  "Returns a copy of L1 with the order of its members reversed"
  (check-type l list)
  (reverse2 l '()))

;;;Exercise 17.10
(defun sub-first (new old l)
  "Returns a copy of L with the element new replacing the first occurence of the element old"
  (check-type new element)
  (check-type old element)
  (check-type l list)
  (cond ((null l) '())
	((eql old (first l)) (cons new (rest l)))
	(t (cons (first l) (sub-first new old (rest l))))))

;;;Exercise 17.12
(defun subst* (new old l)
  "Returns a copy of L with all occurences of OLD relpaced by NEW"
  (check-type new (satisfies util:elementp))
  (check-type old (satisfies util:elementp))
  (cond ((null l) '())
	((eql old (first l)) (cons new (subst* new old (rest l))))
	(t (cons (first l) (subst* new old (rest l))))))

;;;Exercise 17.28
(defun xprod (s1 s2)
  (check-type s1 (satisfies set:setp))
  (check-type s2 (satisfies set:setp))
  (cons :set (xprodh (rest s1) (rest s2))))

(defun xprodh (s1 s2)
  (cond ((null s1) '())
	(t (append (xprod1 (first s1) s2) (xprodh (rest s1) s2)))))

(defun xprod1 (e s)
  (cond ((null s) '())
	(t (cons (list e (first s)) (xprod1 e (rest s))))))
