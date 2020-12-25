;;;; day25.lisp

(in-package #:aoc2020.day25)

(defun loop-size (public-key subject-number) 
  (loop for loop-size from 1
        for value  = subject-number then (rem (* value subject-number) 20201227)
        until (= value public-key)
        finally (return loop-size)))

(defun transform (subject-number loop-size)
  (loop repeat loop-size
        for value = subject-number then (rem (* value subject-number) 20201227)
        finally (return value)))

(defun parse (input)
  (mapcar #'parse-integer (aoc:lines input)))

(defun part1 (input)
  (destructuring-bind (card-public-key door-public-key) (parse input)
    (let ((card-loop-size (loop-size card-public-key 7)))
      (transform door-public-key card-loop-size))))
