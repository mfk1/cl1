(defpackage util)
(in-package :util)

(defun elementp (o)
       "Returns T if O is an element(al object), which is a lisp object that is testable with eql. By definition these are: symbols, characters, numbers and packages"
       (or (symbolp o) (characterp o) (numberp o) (packagep o)))

(export 'elementp)
