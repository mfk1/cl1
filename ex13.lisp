(defpackage test
  (:shadow null consp not))
(in-package test)

;;; Exercise 13.4
(defun ex13_4 (x y)
  (or (= y 0) (> (/ x y) 100)))

;;; Exercise 13.5
(defun ex13_5 (o)
  "Returns True if O is a string/list containing more than five characters/members."
  (and (or (stringp o) (listp o)) (> (length o) 5)))

;;; Exercise 13.7
(defun null (o)
  "Takes any Lisp object and returns True if O is NIL; returns NIL otherwise."
  (eql o NIL))

;;; Exercise 13.9
(defun consp (o)
  "Takes any Lisp object and returns True if the object is a nonempty list;NIL otherwise"
  (and (listp o) (> (length o) 0)))

;;; Exercise 13.10
(defun not (e)
  "Returns True if its argument is NIL; NIL otherwise"
  (eql e NIL))

