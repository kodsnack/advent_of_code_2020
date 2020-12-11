;;;; day10.lisp

(in-package #:aoc2020.day10)

(defun parse (input) (mapcar #'parse-integer (aoc:lines input)))

(defun order (adapters) (sort (copy-seq adapters) #'<))

(defun differences (adapters)
  (loop for (a b) on (order adapters)
        when b collect (- b a)))

(defun computer (adapters) (cons (+ 3 (reduce #'max adapters)) adapters))
(defun outlet (adapters) (cons 0 adapters))

(defun counts (differences)
  (loop for x in differences
        counting (= 1 x) into ones
        counting (= 3 x) into threes
        finally (return (list ones threes))))

(defun part1 (input)
  (apply #'* (counts (differences (outlet (computer (parse input)))))))

(defun arrangements (adapters)
  (loop for a = 0 then x
        and b = 0 then a
        and c = 0 then b
        for i from 0 to (reduce #'max adapters)
        for x = 1 then (if (member i adapters)
                           (+ a b c)
                           0)
        finally (return x)))

(defun part2 (input)
  (arrangements (order (outlet (computer (parse input))))))
