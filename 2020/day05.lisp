;;;; day05.lisp

(in-package #:aoc2020.day05)

(defun parse (line)
  (parse-integer (aoc:tr "FBLR" "0101" line) :radix 2))

(defun seats (input) (mapcar #'parse (aoc:lines input)))

(defun part1 (input) (apply #'max (seats input)))

(defun find-seat (seats)
  (loop for (a b) on (sort seats #'<)
        for s = (1+ a)
        when (< s b) do (return s)))

(defun part2 (input) (find-seat (seats input)))
