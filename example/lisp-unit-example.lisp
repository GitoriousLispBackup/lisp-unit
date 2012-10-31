(defpackage :lisp-unit-example
  (:use :common-lisp)
  (:export #:pick-greater
           #:my-sqrt))

(in-package :lisp-unit-example)

(defun pick-greater (x y)
  (declare (ignore y))
  x)

(defun my-sqrt (n)
  (/ n 2))


