(defpackage util
  (:export elementp))
(in-package util)

;;; Exercise 13.12
(defun elementp (o)
  "Takes any Lisp object and returns True if it is testable with eql;
those objects are symbols, numbers, characters and packages; returns NIL otherwise"
  (or (symbolp o) (numberp o) (characterp o) (packagep o)))
