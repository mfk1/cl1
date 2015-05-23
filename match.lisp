(defpackage match)
(in-package match)

(defun variablep (s)
  "Takes a symbol S and returns True if its first charakter is #\?; NIL otherwise"
  (char= (char (symbol-name s) 0) #\?))
