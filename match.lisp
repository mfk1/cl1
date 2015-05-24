(defpackage match)
(in-package match)

;;; Exercises 12.2 and 13.14
(defun variablep (s)
  "Takes a symbol S and returns True if its first charakter is #\?;
   NIL otherwise"
  (and (symbolp s) (char= (char (symbol-name s) 0) #\?)))

;;; Exercise 13.13
(defun match-element (e1 e2)
  "Takes two Lisp objects and returns True if they are eql or if either of them
is a variable recognized by variablep"
  (or (eql e1 e2) (or (variablep e1) (variablep e2)))) 
