;;;; day02.lisp

(in-package #:aoc2020.day02)

(defun parse (line)
  (ppcre:register-groups-bind ((#'parse-integer min max) ch pw)
      ("([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" line)
    (list min max (elt ch 0) pw)))

(defun check (min max ch pw) (<= min (count ch pw) max))

(defun part1 (input)
  (count-if (lambda (x) (apply #'check x))
            (mapcar #'parse (aoc:lines input))))

(defun check2 (a b ch pw)
  (flet ((cp (p) (count ch pw :start p :end (1+ p))))
    (= 1 (+ (cp (1- a)) (cp (1- b))))))

(defun part2 (input)
  (count-if (lambda (x) (apply #'check2 x))
            (mapcar #'parse (aoc:lines input))))
