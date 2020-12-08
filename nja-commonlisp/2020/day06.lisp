;;;; day06.lisp

(in-package #:aoc2020.day06)

(defun parse (input)
  (mapcar #'answers (ppcre:split "\\n\\n+" input)))

(defun chars (input)
  (loop for ch across input collect ch))

(defun answers (group-input)
  (mapcar #'chars (aoc:lines group-input)))

(defun counts (f l)
  (mapcar (compose #'length (curry #'reduce f)) l))

(defun part1 (input)
  (reduce #'+ (counts #'union (parse input))))

(defun part2 (input)
  (reduce #'+ (counts #'intersection (parse input))))
