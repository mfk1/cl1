(defpackage match)
(in-package match)

;;; The solution of exercise 12.2
(defun variablep (s)
  "Takes a symbol S and returns True if its first charakter is #\?; NIL otherwise"
  (char= (char (symbol-name s) 0) #\?))
