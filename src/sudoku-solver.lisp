;;;; sudoku-solver.lisp - sudoku solver.

(in-package #:sudoku-solver)

(defun sudoku-print (sudoku)
  "Pretty-print SUDOKU."
  (dotimes (row 9)
    (format t "~{~A~^ ~}~%" (sudoku-row sudoku row))))

(defun sudoku-empty-p (object)
  "True if OBJECT represents an empty slot in a sudoku board. An empty slot is marked with a - (dash) string-designator."
  (and (string-designator-p object)
       (string= '- object)))

(defun sudoku-number-p (object)
  "True if OBJECT is a valid number in a sudoku (i.e. 1-9 inclusive)."
  (typep object '(integer 1 9)))

(defun sudoku-elt (sudoku n)
  "Get the value of the slot N in SUDOKU.

See also: `sudoku-at'"
  (elt sudoku n))

(defun sudoku-at (sudoku x y)
  "Get the value of the slot at X,Y in SUDOKU.

See also: `sudoku-elt'"
  (sudoku-elt sudoku (+ x (* y 9))))

(defun sudoku-at (n)
  "Convert an index into a sudoku board into a list of its x,y position."
  (list (mod n 9) (floor n 9)))

(defun sudoku-block (sudoku x y)
  "Get the \"block\" at position X,Y."
  (loop :for y :from (* y 3) :below (* (1+ y) 3)
        :append (loop :for x :from (* x 3) :below (* (1+ x) 3)
                      :collect (sudoku-at sudoku x y))))

(defun sudoku-block-elt (sudoku elt)
  "Get the \"block\" at ELT."
  (sudoku-block sudoku (mod elt 3) (floor elt 3)))

(defun sudoku-cell-block (x y)
  "Get the X,Y coodinates of the specified cell's \"block\"."
  (list (floor x 3) (floor y 3)))

(defun sudoku-row (sudoku row)
  "Get ROW in SUDOKU."
  (subseq sudoku (* 9 row) (* 9 (1+ row))))

(defun sudoku-column (sudoku column)
  "Get COLUMN in SUDOKU."
  (loop :for y :from 0 :upto 8
        :collect (sudoku-at sudoku column y)))

(defun sudoku-possibilities-at (sudoku x y)
  "Get a list of possible answers for the specified cell in SUDOKU."
  (when (sudoku-number-p (sudoku-at sudoku x y))
    (return-from sudoku-possibilities-at (list (sudoku-at sudoku x y))))
  (let ((possibilities (iota 9 :start 1)))
    (dolist (num (append (sudoku-row sudoku y)
                         (sudoku-column sudoku x)
                         (apply #'sudoku-block sudoku (sudoku-cell-block x y)))
                 possibilities)
      (removef possibilities num))))

(defun sudoku-possibilities-elt (sudoku n)
  "Get a list of possible answers for the specified cell index in SUDOKU."
  (apply #'sudoku-possibilities-at sudoku (sudoku-at n)))

(defun sudoku-fill-unambiguous (sudoku)
  "For each empty slot in SUDOKU, fill in its value if there is an unambiguous answer. Repeats until all unambiguous answers are filled in. Note that this will not make any \"guesses\"; in other words, if a slot has multiple possibilities, this function will skip over that slot, only coming back to it if a later answer makes it unambiguous.

See also: `sudoku-solved-p', `sudoku-solve'"
  (assert (length= 81 sudoku) (sudoku))
  (let ((sudoku (copy-list sudoku)))
    (tagbody check
       (dotimes (n 81)
         (when (sudoku-empty-p (sudoku-elt sudoku n))
           (let ((possibilities (sudoku-possibilities-elt sudoku n)))
             (when (length= 1 possibilities)
               (setf (elt sudoku n) (car possibilities))
               (go check))))))
    sudoku))

(defun sudoku-solved-p (sudoku)
  "True if SUDOKU is completely filled.

See also: `sudoku-valid-solution-p'"
  (not (find-if #'sudoku-empty-p sudoku)))

(defun sudoku-all-blocks-columns-and-rows (sudoku)
  "Get a list of all blocks, columns, and rows. This is used for `sudoku-valid-solution-p'."
  (loop :for n :from 0 :to 8
        :collect (sudoku-block-elt sudoku n)
        :collect (sudoku-column sudoku n)
        :collect (sudoku-row sudoku n)))

(defun sudoku-valid-solution-p (sudoku)
  "True if SUDOKU is completely filled, and that the solution is valid.

See also: `sudoku-solved-p'"
  (and (sudoku-solved-p sudoku)
       (loop :for section :in (sudoku-all-blocks-columns-and-rows sudoku)
             :unless (length= 9 (remove-duplicates section))
               :return (progn (format t "Invalid section: ~S" section)
                              nil)
             :finally (return t))))

(defun sudoku-guess-elt-possibility (sudoku elt possibility)
  "Guess POSSIBILITY in cell ELT in SUDOKU."
  (sudoku-possibilities-elt (sudoku-fill-unambiguous sudoku) elt))

(defun sudoku-guess-elt (sudoku &optional (elt 0))
  "Guess all the possibilities at ELT in SUDOKU."
  (let ((possibilities-elt (sudoku-possibilities-elt (sudoku-fill-unambiguous sudoku) elt)))
    (dolist (possibility possibilities-elt)
      (let ((sudoku (copy-list sudoku)))
        (setf (elt sudoku elt) possibility)
        (let ((maybe-solved (sudoku-fill-unambiguous sudoku)))
          (when (sudoku-solved-p maybe-solved)
            (return-from sudoku-guess-elt maybe-solved)))))))

(defun sudoku-guess-all (sudoku)
  "Check all guesses for all unfilled slots in SUDOKU. Note that this only tries one guess at a time. In other words, if the sudoku has multiple levels of ambiguous slots, a solution will not be found."
  (loop :for elt :from 0 :upto 81
        :for maybe-solved := (sudoku-guess-elt sudoku elt)
        :if maybe-solved
          :return maybe-solved))

(defun sudoku-solve (sudoku)
  "Solve SUDOKU."
  (sudoku-guess-all sudoku))
