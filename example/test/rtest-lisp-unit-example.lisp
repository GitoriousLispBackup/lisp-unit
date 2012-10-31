;;; ----------------------------------------------------------------------------
;;; rtest-example.lisp
;;;
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------

(defpackage :lisp-unit-example-test
  (:use :lisp-unit-example :lisp-unit :common-lisp))

(in-package :lisp-unit-example-test)

(define-test pick-greater
  (assert-equal  5 (pick-greater 2 5))
  (assert-equal  5 (pick-greater 5 2))
  (assert-equal 10 (pick-greater 10 10))
  (assert-equal  0 (pick-greater -5 0)))

(define-test pick-greater-2
  (with-test-listener (show-no-result)
    (assert-equal  5 (pick-greater 2 5))
    (assert-equal  5 (pick-greater 5 2))
    (assert-equal 10 (pick-greater 10 10))
    (assert-equal  0 (pick-greater -5 0))))

(define-test my-sqrt
  (dotimes (i 5)
    (assert-equal i (my-sqrt (* i i)))))

(define-test error-1
  (assert-equal 5 (missing 5 2)))

(define-test error-2
  (assert-equal 5 (my-sqrt "string")))

;;; --- rtest-lisp-unit-example.lisp -------------------------------------------
