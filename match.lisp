(defpackage match)
(in-package match)

;;; Exercises 12.2 and 13.14
(defun variablep (s)
  "Takes a symbol S and returns True if its first charakter is #\?;
   NIL otherwise"
  (and (symbolp s) (> (length (symbol-name s)) 1) (char= (char (symbol-name s) 0) #\?)))

;;; Exercise 13.13 and 14.8
(defun match-element (e1 e2)
  "Takes two Lisp objects and returns True if the two arguments are eql or either argument is ?; if either argument is a variable (as recognized by variablep), match-element returns a two-member list whose first member is the variable and whose second member is the other argument; otherwise, match-element returns NIL.
"
  (cond ((variablep e1) (cons e1 (cons e2 '())))
	((variablep e2) (cons e2 (cons e1 '())))
	(t  (or (eql e1 e2) (eql e1 '?) (eql e2 '?)))))

;;; Exercise 14.7
(defun dont-care (s)
  "Returns True if its argument is a question mark symbol; NIL otherwise."
  (if (not (symbolp s))
	   NIL
	   (string= (symbol-name s) "?")))
;;; other solution (replaces the if expression):
;;; (and (symbolp s) (string= (symbol-name s) "?"))

;;;Exercise 17.29
(defun boundpvsubs (v al)
  "Returns True if the variable v is bound to anything in the association list AL; NIL otherwise."
  (check-type v (satisfies match::variablep))
  (check-type al list)
  (if (assoc v al) t
  nil))

;;; Exercise 17.30
(defun boundtovsubs (v al)
  "Takes a variable V (recognized by VARIABLEP) and a substitution SUBS; returns the term that the variable V is bound to in SUBS (if it is); NIL otherwise."
  (check-type v (satisfies match::variablep))
  (check-type al list)
  (cond ((null al) nil)
	((member v (first al)) (first al))
	(t (boundtovsubs v (rest al)))))

