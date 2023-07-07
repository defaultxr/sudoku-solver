;;;; t/sudoku-solver.lisp

(in-package #:sudoku-solver/tests)

(in-suite sudoku-solver-tests)

(defparameter *example-sudokus* '((- - 1 6 3 8 - - 2
                                   4 - 3 9 5 - - - -
                                   - - - 4 7 - 3 1 9
                                   - - 9 - - 7 5 - -
                                   - - 4 - - 5 - - 8
                                   5 1 8 - - 4 - - 7
                                   8 - - - 2 - - 7 4
                                   - - 2 7 4 - - - 5
                                   - 4 7 - 8 9 - 6 -))
  "Various example sudoku boards.")

(test test-example-sudokus
  "Check that the solver can complete the `*example-sudokus*'."
  (dolist (sudoku *example-sudokus*)
    (is (sudoku-valid-solution-p (sudoku-solve sudoku))
        "sudoku-solve generated an incomplete or incorrect solution for ~S" sudoku)))
