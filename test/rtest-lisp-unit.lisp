;;; ----------------------------------------------------------------------------
;;; rtest-lisp-unit.lisp
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

(defpackage :lisp-unit-test
  (:use :lisp-unit :common-lisp))

(in-package :lisp-unit-test)

(define-test lisp-unit-test-failure
  (let ((failure (lisp-unit::make-condition 'lisp-unit::test-failure
                                            :message "A test failure")))
    (assert-equal "A test failure" (lisp-unit::test-failure-message failure))
    (handler-case
      (signal 'lisp-unit::test-failure :message "Signal a failure.")
      (lisp-unit::test-failure (condition)
        (assert-equal
          "Handler-case: Signal a failure."
          (format nil "Handler-case: ~A" (lisp-unit::test-failure-message condition)))))
    (handler-case
      (dotimes (i 1000)
        (when (> i 100)
          (fail "Loop exceeded i = ~A" i)))
      (lisp-unit::test-failure (condition)
        (assert-equal
          "Handler-case: Loop exceeded i = 101"
          (format nil "Handler-case: ~A" (lisp-unit::test-failure-message condition)))))))

(define-test lisp-unit-logically-equal
  (assert-true  (logically-equal t t))
  (assert-true  (logically-equal nil nil))
  (assert-false (logically-equal nil t))
  (assert-false (logically-equal t nil))
  (assert-true  (logically-equal 1 2))
  (assert-true  (logically-equal 1 "str"))
  (assert-false (logically-equal nil 1))
  (assert-false (logically-equal "str" nil)))

(define-test lisp-unit-set-equal
  (assert-true  (set-equal '() '()))
  (assert-true  (set-equal '(1 2 3) '(1 2 3)))
  (assert-true  (set-equal '(1 2 3) '(3 2 1)))
  (assert-true  (set-equal '(1 2 3) '(1 1 2 2 3 3)))
  (assert-true  (set-equal '(1 2 3) '(3 3 3 2 2 1)))
  (assert-true  (set-equal '(a b c) '(b c a) :test #'eq))
  (assert-true  (set-equal '("a" "b" "c") '("b" "c" "a") :test #'eq))
  (assert-true  (set-equal '("a" "b" "c") '("b" "c" "a") :test #'eql))
  (assert-true  (set-equal '("a" "b" "c") '("b" "c" "a") :test #'equal)))

(define-test lisp-unit-unordered-equal
  (assert-true  (unordered-equal '(1 2 3) '(3 2 1)))
  (assert-true  (unordered-equal '(a b c) '(c a b)))
  (assert-true  (unordered-equal '(1 a "a") '("a" 1 a)))
  (assert-true  (unordered-equal '(1 a "a") '("a" 1 a) :test #'eq))
  (assert-true  (unordered-equal '(1 a "a") '("a" 1 a) :test #'eql))
  (assert-false (unordered-equal '(1 2 3) '(3 3 3 2 2 1))))

(define-test lisp-unit-assert-equality
  (assert-equality 'logically-equal t t)
  (assert-equality 'logically-equal nil nil)
  (assert-equality 'logically-equal 1 2)
  (assert-equality 'logically-equal "str" 'a)

  (assert-equality 'set-equal '() '())
  (assert-equality 'set-equal '(1 2 3) '(1 2 3))
  (assert-equality 'set-equal '(1 2 3) '(3 2 1))
  (assert-equality 'set-equal '(a b c) '(c c c b b a))
  (assert-equality 'set-equal '("a" "b" "c") '("c" "c" "c" "b" "b" "a")))

