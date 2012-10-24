;;; ----------------------------------------------------------------------------
;;; lisp-unit.lisp
;;;
;;; This software is a fork of the library lisp-unit written by Chris Riesbeck.
;;;
;;; Copyright (c) 2004-2005 Christopher K. Riesbeck
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

(in-package :cl-user)

(defpackage :lisp-unit
  (:use :common-lisp)
  (:export #:define-test #:run-all-tests #:run-tests
           #:assert-eq #:assert-eql #:assert-equal #:assert-equalp
           #:assert-error #:assert-expands #:assert-false 
           #:assert-equality #:assert-prints #:assert-true
           #:fail
           #:get-test-code #:get-tests
           #:remove-all-tests #:remove-tests
           #:logically-equal #:set-equal #:unordered-equal
           #:use-debugger
           #:with-test-listener
           #:*test-listener*))

(in-package :lisp-unit)

(pushnew :lisp-unit *features*)

;;; ----------------------------------------------------------------------------
;;;
;;; Globals
;;;
;;; ----------------------------------------------------------------------------

(defparameter *tests* (make-hash-table)
  "A hashtable to store the hashtables for the different packages which contain
   the code of the test functions.")

(defparameter *use-debugger* nil
  "If nil, errors in tests are caught and counted.
   If :ask, user is given option of entering debugger or not.
   If true and not :ask, debugger is entered.")

(defparameter *test-listener* nil
  "Stores a function to rebind the test listener.")

(defvar *test-count* 0
  "Used by RUN-TESTS to collect summary statistics.")

(defvar *pass-count* 0
  "Used by RUN-TESTS to collect summary statistics.")

(defvar *test-name* nil
 "Set by RUN-TESTS for use by SHOW-FAILURE")

;;; ----------------------------------------------------------------------------
;;;
;;; Conditions
;;;
;;; ----------------------------------------------------------------------------

(define-condition test-failure (condition)
  ((message :initarg :message
            :reader test-failure-message)))

;;; ----------------------------------------------------------------------------
;;;
;;; Macros
;;;
;;; ----------------------------------------------------------------------------

;; DEFINE-TEST

(defmacro define-test (name &body body)
  "@arg[name]{The name of the test.}
   @arg[body]{The code to be executed.}
   This macro defines a test called @code{name} with the expressions specified,
   in the package specified by the value of @code{*package*} in effect when
   @code{define-test} is executed. The expresssions are assembled into runnable
   code whenever needed by @code{run-tests}. Hence you can define or redefine
   macros without reloading tests using those macros."
  `(progn
     (store-test-code ',name ',body)
     ',name))

;; ASSERT macros

(defun expand-extras (extras)
  `#'(lambda ()
       (list ,@(mapcan #'(lambda (form) (list `',form form)) extras))))

(defun expand-assert (type form body expected extras &key (test #'eql))
  `(internal-assert ,type
                    ',form
                    #'(lambda () ,body)
                    #'(lambda () ,expected)
                    ,(expand-extras extras)
                    ,test))
  
(defun expand-error-form (form)
  `(handler-case ,form
     (condition (error) error)))

(defun expand-output-form (form)
  (let ((out (gensym)))
    `(let* ((,out (make-string-output-stream))
            (*standard-output* (make-broadcast-stream *standard-output* ,out)))
       ,form
       (get-output-stream-string ,out))))

(defun expand-macro-form (form env)
  `(macroexpand-1 ',form ,env))

(defmacro assert-eq (expected form &rest extras)
  "@arg[expected]{the expected value}
   @arg[form]{an expression}
   @arg[extras]{to be printed if the test fails}
   @short{Assertion with the predicate @code{eq}}

   All of the assertion forms are macros. They tally a failure if the associated
   predication returns false. Assertions can be made about return values,
   printed output, macro expansions, and even expected errors. Assertion form
   arguments are evaluated in the local lexical environment.

   All assertion forms allow to include additional expressions at the end of
   the form. These expressions and their values will be printed only when the
   test fails.

   Return values are unspecified for all assertion forms.
   @see{assert-eql}
   @see{assert-equal}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :equal form form expected extras :test #'eq))

(defmacro assert-eql (expected form &rest extras)
  "@arg[expected]{the expected value}
   @arg[form]{an expression}
   @arg[extras]{to be printed if the test fails}
   @short{Assertion with the predicate @code{eql}}

   See @fun{assert-eq} for a detailed description.
   @see{assert-equal}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :equal form form expected extras :test #'eql))

(defmacro assert-equal (expected form &rest extras)
  "@arg[expected]{the expected value}
   @arg[form]{an expression}
   @arg[extras]{to be printed if the test fails}
   @short{Assertion with the predicate @code{equal}}

   See @fun{assert-eq} for a detailed description.
   @see{assert-eql}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :equal form form expected extras :test #'equal))

(defmacro assert-equalp (expected form &rest extras)
  "@arg[expected]{the expected value}
   @arg[form]{an expression}
   @arg[extras]{to be printed if the test fails}
   @short{Assertion with the predicate @code{equalp}}

   See @fun{assert-eq} for a detailed description.
   @see{assert-eql}
   @see{assert-equal}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :equal form form expected extras :test #'equalp))

(defmacro assert-equality (test expected form &rest extras)
  "@arg[test]{a predicate with two arguments}
   @arg[expected]{the expected value}
   @arg[form]{an test form}
   @arg[extras]{to be printed if the test fails}
   @short{Assertion with a user defined predicate.}

   These macros tally a failure if the value @code{expected} is not equal to the
   result returned by @code{form}, using the specified equality predicate
   @code{test}.

   In general, @fun{assert-equal} is used for most tests. But any binary
   predicate can be used, with @code{assert-equality}, e.g.,
   @begin{pre}
   (assert-equality #'unordered-equal 
                    '(a b c) (unique-atoms '((b c) a ((b a) c))))
   @end{pre}
   Besides the predicate @fun{unordered-equal}, the predicates @fun{set-equal}
   and @fun{logically-equal} might be useful.

   See @fun{assert-eq} for a detailed description of assert macros.
   @see{assert-eql}
   @see{assert-equalp}
   @see{assert-true}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :equal form form expected extras :test test))

(defmacro assert-true (form &rest extras)
  "@arg[form]{an test form}
   @arg[extras]{to be printed if the test fails}
   @code{assert-true} tallies a failure if the test form returns false.
   See @fun{assert-eq} for a detailed description of assert macros.
   @see{assert-eql}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-false}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :result form form t extras))

(defmacro assert-false (form &rest extras)
  "@arg[form]{an test form}
   @arg[extras]{to be printed if the test fails}
   @code{assert-false} tallies a failure if the test returns true.
   See @fun{assert-eq} for a detailed description of assert macros.
   @see{assert-eql}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-error}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :result form form nil extras))

(defmacro assert-error (condition form &rest extras)
  "This macro tallies a failure if @code{form} does not signal an error that is
   equal to or a subtype of condition-type. Use error to refer to any kind of
   error. See condition types in the Common Lisp Hyperspec for other possible
   names. For example,
   @begin{pre}
   (assert-error 'arithmetic-error (foo 0))
   @end{pre}
   would assert that @code{foo} is supposed to signal an arithmetic error when
   passed zero.
   See @fun{assert-eq} for a detailed description of assert macros.
   @see{assert-eql}
   @see{assert-equalp}
   @see{assert-equality}
   @see{assert-true}
   @see{assert-false}
   @see{assert-prints}
   @see{assert-expands}"
  (expand-assert :error form (expand-error-form form) condition extras))

(defmacro assert-prints (output form &rest extras)
  "This macro tallies a failure if form does not print to standard output stream
   output equal to the given string, ignoring differences in beginning and
   ending newlines."
  (expand-assert :output form (expand-output-form form) output extras))

(defmacro assert-expands (&environment env expansion form &rest extras)
  "This macro tallies a failure if (macroexpand-1 form) does not produce a value
   equal to expansion."
  (expand-assert :macro
                 form
                 (expand-macro-form form #+lispworks nil #-lispworks env)
                 expansion
                 extras))

;; FAIL

(defun fail (str &rest args)
  "@arg[str]{a format string}
   @arg[args]{the args to be printed}
   @short{Signals a failure.}

   Calling this function tallies a failure. A string describing the failure is
   constructed by calling @code{(format nil format-string [form1 form2 ...])}.
   For example,
   @begin{pre}
    (when (> (length queue) 100)
      (fail ''Queue exceeded expected size by '' (- (length queue) 100)))
   @end{pre}"
  (signal 'test-failure :message (apply #'format nil str args)))

;;; ----------------------------------------------------------------------------
;;;
;;; Useful equality predicates for tests
;;;
;;; ----------------------------------------------------------------------------

(defun logically-equal (x y)
  "@arg[x]{an object}
   @arg[y]{an object}
   @return{@code{T} or @code{NIL}}
   Return @code{T} if @code{x} and @code{y} both are false or both are true.
   @see{set-equal}
   @see{unordered-equal}"
  (eql (not x) (not y)))

(defun set-equal (l1 l2 &key (test #'equal))
  "@arg[l1]{a sequence}
   @arg[l2]{a sequence}
   @arg[test]{a predicate function, the default is @code{equal}}
   @return{@code{T} or @code{NIL}}
   @short{Compare two sequences to have the same elements.}
   Return true if every element of the sequence l1 is an element of the sequence
   l2 and vice versa. The number of elements in the sequences can be different.
   @see{logically-equal}
   @see{unordered-equal}"
  (and (listp l1)
       (listp l2)
       (subsetp l1 l2 :test test)
       (subsetp l2 l1 :test test)))

(defun unordered-equal (l1 l2 &key (test #'equal))
  "@arg[l1]{a sequence}
   @arg[l2]{a sequence}
   @arg[test]{a predicate function, the default is @code{equal}}
   @return{@code{T} or @code{NIL}}
   @short{Compare two sequences to be unordered equal.}
   This predicate returns true if the first sequence is a permutation of the
   second. For example, @code{(unordered-equal '(a b a) '(b a a))} is true, but
   @code{(unordered-equal '(a b a) '(a b a a))} is false.
   The keyword argument @code{:test} can be used to specify an equality
   predicate. The default is @code{equal}.
   @see{set-equal}
   @see{logically-equal}"
  (and (listp l1)
       (listp l2)
       (= (length l1) (length l2))
       (every #'(lambda (element)
                  (= (count element l1 :test test)
                     (count element l2 :test test)))
              l1)))

;;; ----------------------------------------------------------------------------
;;;
;;; Private functions
;;;
;;; ----------------------------------------------------------------------------

;; DEFINE-TEST support

(defun get-package-table (package &key create)
  (let ((table (gethash (find-package package) *tests*)))
    (or table
        (and create
             (setf (gethash package *tests*)
                   (make-hash-table))))))

(defun store-test-code (name code &optional (package *package*))
  (setf (gethash name (get-package-table package :create t))
        code))

;; OUTPUT support

(defun get-failure-message (type)
  (case type
    (:error "~&~@[Should have signalled ~{~S~^; ~} but saw~] ~{~S~^; ~}")
    (:macro "~&Should have expanded to ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (:output "~&Should have printed ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")
    (:failure "~&Expected ~{~S~^; ~} but saw failure: ~A")
    (t "~&Expected ~{~S~^; ~} ~<~%~:;but saw ~{~S~^; ~}~>")))

(defun show-failure (type msg name form expected actual extras)
  (format t "~&~@[~S: ~]~S failed: " name form)
  (format t msg expected actual)
  (format t "~{~&   ~S => ~S~}~%" extras)
  type)

(defun show-summary (name test-count pass-count &optional error-count)
  (format t "~&~A: ~S assertions passed, ~S failed~@[, ~S execution errors~]."
          name pass-count (- test-count pass-count) error-count))

(defun collect-form-values (form values)
  (mapcan #'(lambda (form-arg value)
              (if (constantp form-arg)
                  nil
                (list form-arg value)))
          (cdr form)
          values))

;; ASSERTION support

(defun record-result (passed type form expected actual extras)
  (funcall (or *test-listener* 'default-listener)
           passed
           type
           *test-name*
           form expected
           actual 
           (and extras (funcall extras))
           *test-count*
           *pass-count*))

(defun test-passed-p (type expected actual test)
  (ecase type
    (:error
     (or (eql (car actual) (car expected))
         (typep (car actual) (car expected))))
    (:equal
     (and (<= (length expected) (length actual))
          (every test expected actual)))
    (:macro
     (equal (car actual) (car expected)))
    (:output
     (string= (string-trim '(#\newline #\return #\space) 
                           (car actual))
              (car expected)))
    (:result
     (logically-equal (car actual) (car expected)))))

(defun check-results (type form expected actual extras test)
  (let ((passed (test-passed-p type expected actual test)))
    (when passed
      (incf *pass-count*))
    (record-result passed type form expected actual extras)
    passed))

(defun internal-assert (type form code-thunk expected-thunk extras test)
  (incf *test-count*)
  (let ((expected (multiple-value-list (funcall expected-thunk))))
    (handler-case
        (let ((actual (multiple-value-list (funcall code-thunk))))
          (check-results type form expected actual extras test))
      (test-failure (tf)
                    (record-result nil
                                   :failure 
                                   form 
                                   expected
                                   (test-failure-message tf)
                                   extras)
                    nil))))

(defun default-listener
    (passed type name form expected actual extras test-count pass-count)
  (declare (ignore test-count pass-count))
  (unless passed
    (show-failure type
                  (get-failure-message type)
                  name
                  form
                  expected
                  actual
                  extras)))

;; RUN-TESTS support

(defun use-debugger-p (e)
  (and *use-debugger*
       (or (not (eql *use-debugger* :ask))
           (y-or-n-p "~A -- debug?" e))))

(defun run-test-thunk (*test-name* thunk)
  (if (null thunk)
      (format t "~&    Test ~S not found" *test-name*)
    (prog ((*test-count* 0)
           (*pass-count* 0)
           (error-count 0))
      (handler-bind 
          ((error #'(lambda (e)
                      (let ((*print-escape* nil))
                        (setq error-count 1)
                        (format t "~&    ~S: ~W" *test-name* e))
                      (if (use-debugger-p e) e (go exit)))))
        (funcall thunk)
        (show-summary *test-name* *test-count* *pass-count*))
  exit
      (return (values *test-count* *pass-count* error-count)))))

(defun run-test-thunks (test-thunks)
  (unless (null test-thunks)
    (let ((total-test-count 0)
          (total-pass-count 0)
          (total-error-count 0))
      (dolist (test-thunk test-thunks)
        (multiple-value-bind (test-count pass-count error-count)
            (run-test-thunk (car test-thunk) (cadr test-thunk))
          (incf total-test-count test-count)
          (incf total-pass-count pass-count)
          (incf total-error-count error-count)))
      (unless (null (cdr test-thunks))
        (show-summary 'total
                      total-test-count
                      total-pass-count
                      total-error-count))
      (values))))

;;; ----------------------------------------------------------------------------
;;;
;;; Public functions
;;;
;;; ----------------------------------------------------------------------------

(defun get-test-code (name &optional (package *package*))
  "This function returns the body of the code stored for the test name under
   package. If no package is given, the value of *package* is used."
  (let ((table (get-package-table package)))
    (unless (null table)
      (gethash name table))))

(defun get-tests (&optional (package *package*))
  "This function returns the names of all the tests that have been defined for
   the package. If no package is given, the value of *package* is used."
  (let ((l nil)
        (table (get-package-table package)))
    (cond ((null table) nil)
          (t
           (maphash #'(lambda (key val)
                        (declare (ignore val))
                        (push key l))
                    table)
           (sort l #'string< :key #'string)))))

(defun remove-tests (names &optional (package *package*))
  "This function removes the tests named for the given package. If no package is
   given, the value of *package* is used."
  (let ((table (get-package-table package)))
    (unless (null table)
      (if (null names)
          (clrhash table)
        (dolist (name names)
          (remhash name table))))))

(defun remove-all-tests (&optional (package *package*))
  "This function removes the tests for the given package. If no package is
   given, it removes all tests for the current package. If nil is given, it
   removes all tests for all packages."
  (if (null package)
      (clrhash *tests*)
    (remhash (find-package package) *tests*)))

;; RUN-TESTS

(defmacro run-all-tests (package &rest tests)
  "This macro runs all the tests defined in the specified package and reports
   the results."
  `(let ((*package* (find-package ',package)))
     (run-tests
       ,@(mapcar #'(lambda (test) (find-symbol (symbol-name test) package))
                 tests))))

(defmacro run-tests (&rest names)
  "This macro runs the tests named and reports the results. The package used is
   the value of *package* in effect when the macro is expanded. If no names are
   given, all tests for that package are run."
  `(run-test-thunks (get-test-thunks ,(if (null names)
                                          '(get-tests *package*)
                                          `',names))))

(defun get-test-thunk (name package)
  (assert (get-test-code name package)
          (name package)
          "No test defined for ~S in package ~S" name package)
  (list name (coerce `(lambda () ,@(get-test-code name)) 'function)))

(defun get-test-thunks (names &optional (package *package*))
  (mapcar #'(lambda (name) (get-test-thunk name package))
          names))

(defun use-debugger (&optional (flag t))
  "By default, errors that occur while running tests are simply counted and
   ignored. You can change this behavior by calling @code{use-debugger} with one
   of three possible flag values: @code{t} (the default) means your Lisp's
   normal error handling routines will be invoked when errors occur; @code{:ask}
   means you will be asked what to do when an error occurs, and @code{nil} means
   errors are counted and ignored, i.e., the standard behavior."
  (setq *use-debugger* flag))

;; WITH-TEST-LISTENER

(defmacro with-test-listener (listener &body body)
  "Rebind listeners to use user-defined listeners."
  `(let ((*test-listener* #',listener)) ,@body))
  
;;; --- End of file lisp-unit.lisp ---------------------------------------------
