(load "util.lisp")
(defpackage ch16
  (:shadow length member count nth assoc))
(in-package ch16)

;;; EX 16.1
(defun length (l)
  "Returns the number of the members of L"
  (check-type l list)
  (if (null l) 0     ;termination condition - the length of '() is 0
      (1+ (length (rest l)))))

;;; EX 16.4
(defun member (obj l)
  "Returns T if obj is a member of the list L"
  (check-type l list)
  (cond ((null l) nil)
	((eql obj (first l)) l)
	(t (member obj (rest l)))))

;;; EX 16.6
(defun before (e1 e2 l)
   "Returns T if the element e1 occurs before the element e2"
   (check-type e1 (satisfies util:elementp))
   (check-type e2 (satisfies util:elementp))
   (check-type l list)
   (member e2 (member e1 l)))
   ;;; other solution:
   ;;;(> (length (member e1 l)) (length (member e2 l))))

;;; EX 16.10
(defun count (e l)
  "Returns the number of times that the element e appears as a member of the list L"
  (check-type e (satisfies util:elementp))
  (check-type l list)
  (cond ((null l) 0) ; e appears 0 times as member of '()
	((eql e (first l)) (1+ (count e (rest l))))
	(t (count e (rest l)))))

;;; EX 16.11
(defun equal-lelt (l1 l2)
  "Returns T if the corresponding members of l1 and l2, which must be lists of elements, are eql; NIL otherwise"
  (check-type l1 list)
  (check-type l2 list)
  (cond ((null l1) (null l2))
	((null l2) nil)
	((not (eql (first l1) (first l2))) nil)
	(t (equal-lelt (rest l1) (rest l2)))))

;;; ex 16.12
(defun nth (n l)
  "Returns the nth member of the list L"
  (check-type n integer)
  (check-type l list)
  (cond ((null l) nil)
	((= 0 n) (first l))
	(t (nth (1- n) (rest l)))))

;;; EX 16.13
(defun allbut (n l)
  "Returns the list L omitting the first n members"
  (check-type n integer)
  (check-type l list)
  (cond ((null l) nil)
	((= n 1) (rest l))
	(t (allbut (1- n) (rest l)))))

;;; EX 16.14
(defun assoc (e al)
  "Let be E an element and AL a list all of whose members are lists. assoc returns the first list of AL whose first member is eql to E."
  (check-type e (satisfies util:elementp))
  (check-type al list)
  (cond ((null al) nil)
	((eql (first (first al)) e) (first al))
	(t (assoc e (rest al)))))
