;;;; day03.lisp

(in-package #:aoc2020.day03)

(defun parse (input)
  (let* ((lines (aoc:lines input))
         (cols (length (first lines)))
         (rows (length lines))
         (array (make-array (list rows cols) :element-type 'character)))
    (loop for line in lines
          for y from 0 do
            (loop for ch across line
                  for x from 0
                  do (setf (aref array y x) ch)))
    array))

(defun count-trees (right down trees)
  (loop with (height width) = (array-dimensions trees)
        for row = 0 then (+ row down)
        for col = 0 then (mod (+ col right) width)
        while (< row height)
        count (char= #\# (aref trees row col))))

(defun part1 (input)
  (count-trees 3 1 (parse input)))

(defparameter *slopes* '((1 1) (3 1) (5 1) (7 1) (1 2)))

(defun do-slopes (trees)
  (mapcar (lambda (slope) (count-trees (first slope) (second slope) trees)) *slopes*))

(defun part2 (input)
  (apply #'* (do-slopes (parse input))))
