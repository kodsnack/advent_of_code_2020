;;;; tests.lisp

(in-package #:aoc2018.tests)

(deftest day01 ()
  (is (= 592 (aoc2018.day01::part1 (input-for 2018 1))))
  (is (= 241 (aoc2018.day01::part2 (input-for 2018 1)))))

(deftest day02 ()
  (is (= 6448 (aoc2018.day02::part1 (input-for 2018 2))))
  (is (string= "evsialkqyiurohzpwucngttmf" (aoc2018.day02::part2 (input-for 2018 2)))))
