;;; ----------------------------------------------------------------------------
;;; atdoc-lisp-unit.lisp
;;;
;;; Documentation strings for generating the HTML documentation for lisp-unit.
;;;
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

(in-package :lisp-unit)

(setf (documentation (find-package :lisp-unit) t)
 "@em{lisp-unit} is a Common Lisp library that supports unit testing.  There is
  a long history of testing packages in Lisp, usually called regression
  testers.  More recent packages in Lisp and other languages have been inspired
  by JUnit for Java.  For more information on both unit testing and JUnit,
  visit @a[http://www.junit.org]{JUnit.org}.
  @begin[Overview]{section}
    The main goal for @em{lisp-unit} is to make it simple to use.  The
    advantages of @em{lisp-unit} are:
    @begin{itemize}
      @item{written in portable Common Lisp,}
      @item{dead-simple to define and run tests,}
      @item{supports redefining functions and even macros without reloading
        tests,}
      @item{supports test-first programming,}
      @item{supports testing return values, printed output, macro expansions,
        and error conditions,}
      @item{produces short readable output with a reasonable level of detail, and}
      @item{groups tests by package for modularity.}
    @end{itemize}
  @end{section}
  @begin[How to Use lisp-unit]{section}
    @b{Loading the file @code{lisp-unit.lisp}}@break{}
    There are two ways to load @em{lisp-unit}.  The first method is simply to
    load the file @code{lisp-unit.lisp}. The steps are the following:
    @begin{itemize}
      @item{Load (or compile and load) @code{lisp-unit.lisp}.}
      @item{Evaluate @code{(use-package :lisp-unit)}.}
      @item{Load a file of tests. See below for how to define tests.}
      @item{Run the tests with @fun{run-tests}.}
    @end{itemize}
    Any test failures will be printed, along with a summary of how many tests
    were run, how many passed, and how many failed.
    You define a test with the macro @code{define-test}:
    @begin{pre}
  (define-test name exp1 exp2 ...)
    @end{pre}
    This defines a test called @code{name}.  The expressions @code{exp1},
    @code{exp2}, ... can be anything, but typically most will be assertion
    forms.  Tests can be defined before the code they test, even if they are
    testing macros.  This is to support test-first programming.  After defining
    your tests and the code they test, run the tests with
    @begin{pre}
  (run-tests)
    @end{pre}
    This runs every test defined in the current package.  To run just certain
    specific tests, use:
    @begin{pre}
  (run-tests name1 name2 ...)
    @end{pre}
    @b{Loading lisp-unit and executing tests with @code{asdf}}@break{}
    The second method is to load @em{lisp-unit} with the @code{asdf} facility:
    @begin{pre}
  (asdf:load-system :lisp-unit)
    @end{pre}
    Furthermore, with @code{asdf} the execution of the tests can be performed
    with the call
    @begin{pre}
  (asdf:test-system <package-name>)
    @end{pre}
    Here @code{<package-name>} is the package which is tested.  To
    allow this feature of @code{asdf} a method @code{perform} has to be
    added to the system definition.  This is an example for the package
    @em{lisp-unit} itself:
    @begin{pre}
  (asdf:defsystem :lisp-unit
    :name 'lisp-unit'
    :author 'Dieter Kaiser'
    :license 'LLGPL'
    :serial t
    :depends-on ()
    :components ((:file 'lisp-unit'))
    :in-order-to ((asdf:test-op (asdf:load-op :lisp-unit-test))))

  (defmethod perform ((o test-op) (c (eql (find-system :lisp-unit))))
    (eval `(,(intern (string '#:run-all-tests) :lisp-unit) :lisp-unit-test)))
    @end{pre}
    The test package for @em{lisp-unit} has the name @code{lisp-unit-test} and
    has the following system definition:
    @begin{pre}
  (asdf:defsystem :lisp-unit-test
    :depends-on (:lisp-unit)
    :version '1.0.0'
    :components ((:module 'test'
                  :components ((:file 'rtest-lisp-unit')))))
    @end{pre}
    In this example the test files are expected to be in a subdirectory
    @code{test/} of the package itself.
    Because of these definitions tests in the test file
    @code{rtest-lisp-unit.lisp} for the package @em{lisp-unit} can
    be executed with the command:
    @begin{pre}
  (asdf:test-system :lisp-unit)
    @end{pre}
  @end{section}
  @begin[Example]{section}
    The following example
    @begin{itemize}
      @item{defines some tests to see if @code{pick-greater} returns the larger
        of two arguments,}
      @item{defines a deliberately broken version of @code{pick-greater}, and}
      @item{runs the tests.}
    @end{itemize}
    First, we define some tests.
    @begin{pre}
  > (in-package :cs325-user)
  #<PACKAGE CS325-USER>
  > (define-test pick-greater
      (assert-equal 5 (pick-greater 2 5))
      (assert-equal 5 (pick-greater 5 2))
      (assert-equal 10 (pick-greater 10 10))
      (assert-equal 0 (pick-greater -5 0))
    )
    PICK-GREATER
      @end{pre}
      Following good test-first programming practice, we run these tests before
      writing any code.
      @begin{pre}
  > (run-tests pick-greater)
      PICK-GREATER: Undefined function PICK-GREATER called with arguments (2 5).
      @end{pre}
      This shows that we need to do some work. So we define our broken version
      of @code{pick-greater}.
      @begin{pre}
  > (defun pick-greater (x y) x)  ;; deliberately wrong
  PICK-GREATER
      @end{pre}
      Now we run the tests again:
      @begin{pre}
  > (run-tests pick-greater)
  PICK-GREATER: (PICK-GREATER 2 5) failed: Expected 5 but saw 2
  PICK-GREATER: (PICK-GREATER -5 0) failed: Expected 0 but saw -5
  PICK-GREATER: 2 assertions passed, 2 failed.
    @end{pre}
    This shows two failures.  In both cases, the equality test returned
    @code{nil}.  In the first case it was because @code{(pick-greater 2 5)}
    returned 2 when 5 was expected, and in the second case, it was because
    @code{(pick-greater -5 0)} returned -5 when 0 was expected.
  @end{section}
  @begin[Assertion Forms]{section}
    The most commonly used assertion form is
    @begin{pre}
  (assert-equal value form)
    @end{pre}
    This tallies a failure if form returns a value not equal to value. Both
    value and test are evaluated in the local lexical environment. This means
    that you can use local variables in tests. In particular, you can write
    loops that run many tests at once:
    @begin{pre}
  > (define-test my-sqrt
    (dotimes (i 5)
      (assert-equal i (my-sqrt (* i i)))))
  MY-SQRT

  > (defun my-sqrt (n) (/ n 2))   ;; wrong!!

  > (run-tests my-sqrt)
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 1 but saw 1/2
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 3 but saw 9/2
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 4 but saw 8
  MY-SQRT: 2 assertions passed, 3 failed.
    @end{pre}
    However, the above output doesn't tell us for which values of @code{i} the
    code failed. Fortunately, you can fix this by adding expressions at the
    end of the assert-equal. These expression and their values will be
    printed on failure.
    @begin{pre}
  > (define-test my-sqrt
    (dotimes (i 5)
      (assert-equal i (my-sqrt (* i i)) i)))  ;; added i at the end
  MY-SQRT
  > (run-tests my-sqrt)
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 1 but saw 1/2
     I => 1
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 3 but saw 9/2
     I => 3
  MY-SQRT: (MY-SQRT (* I I)) failed: Expected 4 but saw 8
     I => 4
  MY-SQRT: 2 assertions passed, 3 failed.
      @end{pre}
      The next most useful assertion form is
      @begin{pre}
  (assert-true test)
    @end{pre}
    This tallies a failure if test returns false. Again, if you need to print
    out extra information, just add expressions after test.
    There are also assertion forms to test what code prints, what errors code
    returns, or what a macro expands into. A complete list of assertion forms
    is in the reference section.
    Do not confuse assertion forms with Common Lisp's assert macro. assert is
    used in code to guarantee that some condition is true. If it isn't, the
    code halts.
  @end{section}
  @begin[How to Organize Tests with Packages]{section}
    Tests are grouped internally by the current package, so that a set of
    tests can be defined for one package of code without interfering with
    tests for other packages.
    If your code is being defined in cl-user, which is common when learning
    Common Lisp, but not for production-level code, then you should define
    your tests in cl-user as well.
    If your code is being defined in its own package, you should define your
    tests either in that same package, or in another package for test code.
    The latter approach has the advantage of making sure that your tests have
    access to only the exported symbols of your code package.
    For example, if you were defining a date package, your date.lisp file
    would look like this:
    @begin{pre}
  (defpackage :date
    (:use :common-lisp)
    (:export #:date->string #:string->date))

  (in-package :date)

  (defun date->string (date) ...)
  (defun string->date (string) ...)
      @end{pre}
      Your date-tests.lisp file would look like this:
      @begin{pre}
  (defpackage :date-tests
    (:use :common-lisp :lisp-unit :date))

  (in-package :date-tests)

  (define-test date->string
    (assert-true (string= ... (date->string ...)))
    ...)
  ...
    @end{pre}
    You could then run all your date tests in the test package:
    @begin{pre}
  (in-package :date-tests)

  (run-tests)
    @end{pre}
    Alternately, you could run all your date tests from any package with:
    @begin{pre}
  (lisp-unit:run-all-tests :date-tests)
    @end{pre}
  @end{section}
  @begin[Functions and macros for managing tests]{section}
    @aboutmacro{define-test}
    @aboutfun{get-tests}
    @aboutfun{get-test-code}
    @aboutfun{remove-tests}
    @aboutfun{remove-all-tests}
    @aboutmacro{run-all-tests}
    @aboutmacro{run-tests}
    @aboutfun{use-debugger}
  @end{section}
  @begin[Macros for assertions]{section}
    @aboutmacro{assert-eq}
    @aboutmacro{assert-eql}
    @aboutmacro{assert-equal}
    @aboutmacro{assert-equalp}
    @aboutmacro{assert-equality}
    @aboutmacro{assert-true}
    @aboutmacro{assert-false}
    @aboutmacro{assert-prints}
    @aboutmacro{assert-expands}
    @aboutmacro{assert-error}
    @aboutfun{fail}
  @end{section}
  @begin[Utility predicates]{section}
    @aboutfun{logically-equal}
    @aboutfun{set-equal}
    @aboutfun{unordered-equal}
  @end{section}
  @begin[Listeners]{section}
    @em{lisp-unit} calls three listener functions to report test results and
    summary statistics. These functions can be rebound using the facilities
    below.

    @b{Test Listener}@break{}
    The test listener is called after each assertion form in a test is executed.
    The listener is passed
    @begin{itemize}
      @item{the truth or falsity of the assertion}
      @item{a keyword for type of assertion; current valid values for type are:
        @begin{table}
          @entry[:ERROR]{for ASSERT-ERROR}
          @entry[:MACRO]{for ASSERT-EXPANDS}
          @entry[:OUTPUT]{for ASSERT-PRINTS}
          @entry[:RESULT]{for ASSERT-TRUE, ASSERT-FALSE}
          @entry[:FAILURE]{for FAIL calls}
          @entry[:EQUAL]{for all other assertions}
        @end{table}}
      @item{the name of test in which assertion occurred}
      @item{the form tested}
      @item{the expected value}
      @item{the actual value}
      @item{any extra arguments passed to assertion form}
      @item{how many assertions evaluated so far}
      @item{how many assertions passed so far}
    @end{itemize}
    Two test listener functions are exported:
    @begin{table}
      @entry[SHOW-FAILURE-RESULT]{the default, only prints when an assertion
        fails. It prints the form, expected and actual values, and the values of
        any extra forms.}
      @entry[SHOW-NO-RESULT]{never prints anything.}
    @end{table}
    @b{Error Listener}@break{}
    The error listener is called when an error occurs in running a test. The
    listener is passed
    @begin{itemize}
      @item{the error object}
      @item{the name of the test}
    @end{itemize}
    Three error listener functions are exported:
    @begin{table}
      @entry[SHOW-ERROR]{the default, prints the error message. Further
        execution of the test's forms is terminated.}
      @entry[COUNT-ERROR]{prints nothing but the error count is incremented.
        Further execution of the test's forms is terminated.}
      @entry[DEBUG-ERROR]{causes the Lisp debugger to be invoked so that the
        stack can be examined.}
    @end{table}
    @b{Summary Listener}@break{}
    The summary listener is called after
    @begin{itemize}
      @item{all forms in a test have been executed}
      @item{all tests in a package have been run}
      @item{all tests in all packages have been run}
    @end{itemize}
    The listener is passed:
    @begin{itemize}
      @item{the name of the test or a package just finished}
      @item{the number of assertions evaluated}
      @item{the number that passed}
      @item{the number of errors that occurred}
    @end{itemize}
    Three summary listener functions are exported:
    @begin{table}
      @entry[SHOW-SUMMARY]{the default, prints the summaries at both the test
        and package level.}
      @entry[SHOW-PACKAGE-SUMMARY]{prints the summaries at only the package
        level.}
      @entry[SHOW-NO-SUMMARY]{never prints summaries.}
    @end{table}
    @b{Rebinding Listeners}@break{}
    The three listeners are stored in the exported global variables
    *test-listener*, *error-listener*, *summary-listener*. So one way to change
    listeners is with let. For example, to show only package-level summary
    counts:
    @begin{pre}
  (let ((*summary-listener* 'show-package-summary))
    (run-tests))
    @end{pre}
    The above would still show failures and error messages. To hide those and
    just get the counts:
    @begin{pre}
  (let ((*error-listener* 'count-error)
        (*summary-listener* 'show-package-summary)
        (*test-listener* 'show-no-result))
    (run-tests))
    @end{pre}
    To show no individual test results and only package summaries with failures,
    we need to define a function that checks the number of failures.
    @begin{pre}
  (defun show-failing-package (name test-count pass-count error-count)
    (when (or (< pass-count test-count) (> error-count 0))
      (show-summary name test-count pass-count error-count)))

  (let ((*error-listener* 'count-error)
        (*summary-listener* 'show-failing-package)
        (*test-listener* 'show-no-result))
    (run-tests))
    @end{pre}
    @b{with-listeners}@break{}
    A simpler way to rebind listeners is with with-listeners. The second example
    above could be done with:
    @begin{pre}
  (with-listeners (count-error show-package-summary show-no-result)
    (run-tests))
    @end{pre}
    To use user-defined listeners, you first say what listener variable they're
     for with set-listener-variable. The third example above could be done with:
    @begin{pre}
  (defun show-failing-package (name test-count pass-count error-count)
    (when (or (< pass-count test-count) (> error-count 0))
      (show-summary name test-count pass-count error-count)))

  (set-listener-variable 'show-failing-package '*summary-listener*)

  (with-listeners (count-error show-failing-package show-no-result)
    (run-tests))
    @end{pre}
  @end{section}
")

;;; --- End of file atdoc-lisp-unit.lisp ---------------------------------------
