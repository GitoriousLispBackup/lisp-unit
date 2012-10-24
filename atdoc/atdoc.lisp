(asdf:load-system :atdoc)
(asdf:load-system :lisp-unit)

(load "atdoc-lisp-unit.lisp")

(defpackage :atdoc-lisp-unit
  (:use :lisp-unit :common-lisp))

(in-package :atdoc-lisp-unit)

(defun generate-html ()
  (let* ((base (asdf:component-pathname (asdf:find-system :lisp-unit)))
         (output-directory (merge-pathnames "atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:lisp-unit)
      output-directory
      :index-title "lisp-unit"
      :heading "lisp-unit"
      :css "crategus.css"
      :logo nil
      :single-page-p nil
      :include-slot-definitions-p t
      :include-internal-symbols-p t)))

(defun generate-html-single-page ()
  (let* ((base (asdf:component-pathname (asdf:find-system :lisp-unit)))
         (output-directory (merge-pathnames "atdoc/" base)))
    (ensure-directories-exist output-directory)
    (atdoc:generate-html-documentation
      '(:lisp-unit)
      output-directory
      :index-title "lisp-unit"
      :heading "lisp-unit"
      :css "crategus.css"
      :logo nil
      :single-page-p t
      :include-slot-definitions-p t
      :include-internal-symbols-p t)))

(generate-html)

