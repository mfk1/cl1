(defpackage ch16
(:shadow length member count nth assoc))
(in-package ch16)
(load "util.lisp")

;;; Exercise 16.1
(defun length (l)
  "Returns the number of the members of a given list L."
  (check-type l list)
  (cond ((null l) 0)
	(t (1+ (length (rest l))))))

;;; Exercise 16.4
(defun member (o l)
  "Returns True if O is a member of a given list L."
  (check-type l list)
  (cond ((null l) nil)
	((eql o (first l)) l)
	(t (member o (rest l)))))

;;; Exercise 16.6
(defun before (e1 e2 l)
  "Returns True if the element e1 occurs before element e2 (e1, e2 recognized as elements by util:elementp); False otherwise."
  (check-type e1 (satisfies util:elementp))
  (check-type e2 (satisfies util:elementp))
  (check-type l list)
  (> (length (member e1 l)) (length (member e2 l))))

;;; Exercise 16.7
(defun number-listp (l)
  "Returns True if a given list L consists entirely of numbers; NIL otherwise."
  (check-type l list)
  (cond ((null l) t)
	((numberp (first l)) (number-listp (rest l)))
	(t nil)))

;;; Exercise 16.8
(defun same-length1 (l1 l2)
  "Returns True if the lists L1 and L2 have the same length; NIL otherwise."
  (check-type l1 list)
  (check-type l2 list)
  (= (length l1) (length l2)))

(defun same-length2 (l1 l2)
  "Returns True if the lists L1 and L2 have the same length; NIL otherwise."
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
	(t (same-length2 (rest l1) (rest l2)))))

;;; Exercise 16.10
(defun count (e l)
  "Returns the number of times that the element E appears in the list L."
  (check-type e (satisfies util:elementp))
  (check-type l list)
  (cond ((null l) 0)
	((eql e (first l)) (1+ (count e (rest l))))
	(t (count e (rest l)))))

;;; Exercise 16.11
(defun equal-lelt (l1 l2)
  "Returns True if two lists, entirely consisting of elements are eql (i.e. the corresponding members of l1 and l2 are eql); NIL otherwise."
  (check-type l1 (satisfies ch16::leltp)); make sure that all members of l1 are elements.
  (check-type l2 (satisfies ch16::leltp)); make sure that all members of l2 are elements.
  (cond ((null l1) (null l2))
	((null l2) nil)
	((eql (first l1) (first l2)) (equal-lelt (rest l1) (rest l2)))
	(t nil)))
  
(defun leltp (l)
  "Returns True if L is a list that entirely consists of elements."
  (check-type l list)
  (cond ((null l) t)
	((util:elementp (first l)) (leltp (rest l)))
	(t NIL)))

;;; Exercise 16.12
(defun nth (n l)
  "Takes an integer N and a list L and returns its nth member."
  (check-type n integer)
  (check-type l list)
  (cond ((null l) nil)
	((= n 0) (first l))
	(t (nth (1- n) (rest l)))))

;;; Exercise 16.13
(defun allbut (n l)
  "Takes an integer N and a list L and returns a list just like L but omitting its first n members."
  (check-type n integer)
  (check-type l list)
  (cond ((null l) nil)
	((= n 1) (rest l))
	(t (allbut (1- n) (rest l)))))

;;; Exercise 16.14
(defun assoc (e al)
  "Takes an element E and a list AL (a list of lists) and returns its first element whose first member is eql to E."
  (check-type e (satisfies util:elementp))
  (check-type al list)
  (cond ((null al) nil)
	((eql e (first (first al))) (first al))
	(t (assoc e (rest al)))))

;;; Exercise 16.15
(defun match-lelt (l1 l2)
  "Returns True if two lists of elements L1, L2 are eql while considering the symbol ? eql to anything."
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((eql (first l1) (first l2)) (match-lelt (rest l1) (rest l2)))
	((or (eql '? (first l1)) (eql '? (first l2))) (match-lelt (rest l1) (rest l2)))
	(t nil)))

