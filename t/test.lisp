;;;; t/test.lisp - basic tests and test utilities/fixtures/etc for the sudoku-solver test suite.

(defpackage #:sudoku-solver/tests
  (:use #:cl
        #:sudoku-solver
        #:alexandria
        #:mutility
        #:fiveam))

(in-package #:sudoku-solver/tests)

(def-suite sudoku-solver-tests
  :description "sudoku-solver tests suite.")

(in-suite sudoku-solver-tests)

(test system-attributes
  "Check that the system has all the standard attributes"
  (let ((missing (system-missing-attributes '#:sudoku-solver)))
    (is-false missing
              "The system definition is missing attributes: ~S" missing)))

(test undocumented-symbols
  "Check for any undocumented exported symbols"
  (let ((undocumented (package-undocumented-symbols '#:sudoku-solver)))
    (is-false undocumented
              "some exported symbols do not have docstrings: ~S" undocumented)))

(test docstrings-broken-links
  "Check for any broken links in docstrings of exported symbols"
  (let ((symbols (package-docstrings-with-broken-links '#:sudoku-solver)))
    (is-false symbols
              "some exported symbols have docstrings that contain broken links: ~S" symbols)))
