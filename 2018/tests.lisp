;;;; tests.lisp

(in-package #:aoc2018.tests)

(deftest day01 ()
  (is (= 592 (aoc2018.day01::part1 (input-for 2018 1))))
  (is (= 241 (aoc2018.day01::part2 (input-for 2018 1)))))

(deftest day02 ()
  (is (= 6448 (aoc2018.day02::part1 (input-for 2018 2))))
  (is (string= "evsialkqyiurohzpwucngttmf" (aoc2018.day02::part2 (input-for 2018 2)))))

(deftest day03 ()
  (is (= 110546 (aoc2018.day03::part1 (input-for 2018 3))))
  (is (= 819 (aoc2018.day03::part2 (input-for 2018 3)))))

(deftest day04 ()
  (is (= 103720 (aoc2018.day04::part1 (input-for 2018 4))))
  (is (= 110913 (aoc2018.day04::part2 (input-for 2018 4)))))

(deftest day05 ()
  (is (= 11042 (aoc2018.day05::part1 (input-for 2018 5))))
  (is (= 6872 (aoc2018.day05::part2 (input-for 2018 5)))))
