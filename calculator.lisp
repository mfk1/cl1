(defpackage calculator)
(in-package calculator)

;;; The solution of exercise 12.6
(defun combine-expr (o1 o2 l)
  "Takes an arithmetic operator, an operand and a list representing an arithmetic operation and returns 
  the expression with the operator and operand applied to the first member of the expression.
  So (combine-expr ’+ '3 ’(5 - 6 * 8)) should evaluate to ((3 + 5) - 6 * 8)."
  (cons (list o2 o1 (first l)) (rest l)))
