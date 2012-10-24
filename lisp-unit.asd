;;; ----------------------------------------------------------------------------
;;; lisp-unit.asd
;;;
;;; This software is an extension of the library written by Chris Riesbeck.
;;;
;;; Copyright (C) 2004-2005 Christopher K. Riesbeck
;;; Copyright (C) 2012 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining 
;;; a copy of this software and associated documentation files (the "Software"), 
;;; to deal in the Software without restriction, including without limitation 
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense, 
;;; and/or sell copies of the Software, and to permit persons to whom the 
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included 
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
;;; OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------

(defpackage :lisp-unit-asdf
  (:use :asdf :common-lisp))

(in-package :lisp-unit-asdf)

(defsystem :lisp-unit
  :name "lisp-unit"
  :version "1.0.0"
  :author "Dieter Kaiser"
  :license "LLGPL"
  :serial t
  :depends-on ()
  :components ((:file "lisp-unit"))
  :in-order-to ((test-op (load-op :lisp-unit-test))))

;;; ----------------------------------------------------------------------------

(defsystem :lisp-unit-test
  :version "1.0.0"
  :depends-on (:lisp-unit)
  :components ((:module "test"
                :components ((:file "rtest-lisp-unit")))))

(defmethod perform ((o test-op) (c (eql (find-system :lisp-unit))))
  (eval `(,(intern (string '#:run-all-tests) :lisp-unit) :lisp-unit-test)))

;;; --- End of file lisp-unit.asd ----------------------------------------------
