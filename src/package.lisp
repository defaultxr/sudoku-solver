;;;; package.lisp

(uiop:define-package #:sudoku-solver
  (:use #:cl
        #:alexandria
        #:mutility)
  (:export
   #:sudoku-print
   #:sudoku-elt
   #:sudoku-at
   #:sudoku-block
   #:sudoku-block-elt
   #:sudoku-row
   #:sudoku-column
   #:sudoku-possibilities-at
   #:sudoku-possibilities-elt
   #:sudoku-fill-unambiguous
   #:sudoku-solved-p
   #:sudoku-valid-solution-p
   #:sudoku-guess-elt-possibility
   #:sudoku-guess-elt
   #:sudoku-guess-all
   #:sudoku-solve))
