;;;; sudoku-solver.asd

(defsystem #:sudoku-solver
  :name "sudoku-solver"
  :description "A simple sudoku solver."
  :version "0.1"
  :author "modula t."
  :license "MIT"
  :homepage "https://github.com/defaultxr/sudoku-solver"
  :bug-tracker "https://github.com/defaultxr/sudoku-solver/issues"
  :mailto "defaultxr at gmail dot com"
  :source-control (:git "git@github.com:defaultxr/sudoku-solver.git")
  :depends-on (#:alexandria
               #:mutility)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "sudoku-solver"))
  :in-order-to ((test-op (test-op "sudoku-solver/tests"))))

(defsystem #:sudoku-solver/tests
  :name "sudoku-solver/tests"
  :description "FiveAM-based test suite for sudoku-solver."
  :author "modula t."
  :license "MIT"
  :depends-on (#:sudoku-solver
               #:fiveam
               #:mutility/test-helpers)
  :pathname "t/"
  :serial t
  :components ((:file "test")
               (:file "sudoku-solver"))
  :perform (test-op (op c)
                    (uiop:symbol-call :fiveam :run!
                                      (uiop:find-symbol* '#:sudoku-solver-tests
                                                         :sudoku-solver/tests))))

