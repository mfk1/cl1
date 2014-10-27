(load "util")
(defpackage set
	    (:shadow lisp:set lisp:union lisp:first lisp:rest lisp:intersection lisp:complement lisp:subsetp lisp:equal)
	    (:export setp set makeset first rest insert empty intersection complement subsetp equal)
	    (:import-from util bag element))
(in-package set)

;;;Exercise 17.13
;;;(defun setp (l)
;;;  "Returns T if there are no two elements of L which are EQL"
;;;  (if (not (listp l)) nil
;;;  (cond ((null l) t)
;;;	((member (first l) (rest l)) nil)
;;;	(t (setp (rest l))))))

;;;Exercise 17.19
(defun setp (l)
  "Returns T if the given list L is a set, False otherwise"
  (if (not (listp l)) nil ; setp must return False if l is not a list
      (if (eql (lisp:first l) :set) t
	  nil)))

;;;Exercise 17.14
(deftype set ()
  "A set is a list in which no two elements are EQL"
  '(satisfies setp))

;;;Exercise 17.15
;;;(defun makeset (b)
;;;  "Returns a set, just containing the elements of input bag b"
;;;  (check-type b bag)
;;;  (cond ((null b) '())
;;;	((member (first b) (rest b)) (makeset (rest b)))
;;;	(t (cons (first b) (makeset (rest b))))))

;;;Exercise 17.16
;;;Does not work with the definition of a set as a list without any elements being EQL and which is labelled with :set
;;;(defun union (s1 s2)
;;;  "Returns the union of the sets S1 and S2"
;;;  (check-type s1 set)
;;;  (check-type s2 set)
;;;  (cond ((null s1) s2)
;;;	((member (first s1) s2) (union (rest s1) s2))
;;;	(t (cons (first s1) (union (rest s1) s2)))))

;;;Exercise17.23
(defun union (s1 s2)
  "Returns the union of two given sets s1, s2"
  (check-type s1 set)
  (check-type s2 set)
  (cons :set (make-unlabelled-union (lisp:rest s1) (lisp:rest s2))))

(defun make-unlabelled-union (s1 s2)
  "Helper function for UNION"
  (cond ((null s1) s2)
	((member (lisp:first s1) s2) (make-unlabelled-union (lisp:rest s1) s2))
	(t (cons (lisp:first s1) (make-unlabelled-union (lisp:rest s1) s2)))))	

;;;Exercise 17.18
;;;Redefined because of set:first and set:rest
(defun makeset (b)
  "Takes an input bag B and returns a set containing of its members"
  (check-type b bag)
  (cons :set (make-unlabelled-set b)))

(defun make-unlabelled-set (b)
  "Returns a set, just containing the elements of the input bag B"
  (cond ((null b) '())
	((member (lisp:first b) (lisp:rest b)) (make-unlabelled-set (lisp:rest b)))
	(t (cons (lisp:first b) (make-unlabelled-set (lisp:rest b))))))

;;;Exercise 17.20
(defun first (s)
  "Returns the first element of a given set S"
  (check-type s set)
  (second s))

(defun rest (s)
  "Returns the rest of a given set S"
  (check-type s set)
  (cddr s)) ;looks better than (lisp:rest (lisp:rest s))

;;;Exercise 17.21
(defun insert (e s)
  "Takes an element E and a set S, checks if S already contains E and inserts it, if it doesn't. Otherwise it returns S unchanged."
  (check-type e element)
  (check-type s set)
  (if (member e s) s
      (cons (lisp:first s) (cons e (lisp:rest s)))))

;;;Exercise 17.22
(defun empty (s)
  "Returns True if a given set contains no elements; False otherwise"
  (check-type s set)
  (if (= (length s) 1) t
      nil))

;;;Exercise 17.24
(defun intersection (s1 s2)
  "Takes two sets and returns their intersection"
  (check-type s1 set)
  (check-type s2 set)
  (intersection1 s1 s2))

(defun intersection1 (s1 s2)
  "Intersections helper function"
  (cond ((null s1) '())
	((not (member (lisp:first s1) s2)) (intersection1 (lisp:rest s1) s2))
	 (t (cons (lisp:first s1) (intersection1 (lisp:rest s1) s2)))))

(defun complement (s1 s2)
  "Returns the complement of two given sets"
  (check-type s1 set)
  (check-type s2 set)
  (cons :set (complement1 s1 s2)))

(defun complement1 (s1 s2)
  "Complements helper"
  (cond ((null s1) '())
	((member (lisp:first s1) s2) (complement1 (lisp:rest s1) s2))
	(t (cons (lisp:first s1) (complement1 (lisp:rest s1) s2)))))

;;;Exercise 17.26
(defun subsetp (s1 s2)
  "Takes two sets and returns True if S1 is a subset of S2, Nil otherwise"
  (check-type s1 set)
  (check-type s2 set)
  (subsetp1 s1 s2))
  

(defun subsetp1 (s1 s2)
  "Subsets' helper"
  (cond ((null s1) t)
	((not (member (lisp:first s1) s2)) nil)
	(t (subsetp1 (lisp:rest s1) s2))))

;;;Exercise 17.27
(defun equal (s1 s2)
  "Returns True if two given sets S1, S2 are equal, Nil otherwise. Two sets are defined to be equal, if they have exactly the same elements. Neither the order of the elements of the sets, nor the order of the sets as given arguments is relevant for their equality."
  (check-type s1 set)
  (check-type s2 set)
  (if (= (length s1) (length s2)) (equal1 s1 s2)
      nil))

(defun equal1 (s1 s2)
      (cond ((null s1) t)
	    ((not (member (lisp:first s1) s2)) nil)
	    (t (equal1 (lisp:rest s1) s2))))
