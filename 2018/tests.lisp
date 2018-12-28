;;;; tests.lisp

(in-package #:aoc2018.tests)

(deftest day01 ()
  (is (aoc2018.day01::part1 (input-for 2018 1)))
  (is (aoc2018.day01::part2 (input-for 2018 1))))
