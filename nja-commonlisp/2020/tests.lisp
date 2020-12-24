;;;; tests.lisp

(in-package #:aoc2020.tests)

(deftest day01 ()
  (is (= 633216 (aoc2020.day01::part1 (input-for 2020 01))))
  (is (= 68348924 (aoc2020.day01::part2 (input-for 2020 01)))))

(deftest day02 ()
  (is (= 517 (aoc2020.day02::part1 (input-for 2020 02))))
  (is (= 284 (aoc2020.day02::part2 (input-for 2020 02)))))

(deftest day03 ()
  (is (= 276 (aoc2020.day03::part1 (input-for 2020 03))))
  (is (= 7812180000 (aoc2020.day03::part2 (input-for 2020 03)))))

(deftest day04 ()
  (is (= 196 (aoc2020.day04::part1 (input-for 2020 04))))
  (is (= 114 (aoc2020.day04::part2 (input-for 2020 04)))))

(deftest day05 ()
  (is (= 855 (aoc2020.day05::part1 (input-for 2020 05))))
  (is (= 552 (aoc2020.day05::part2 (input-for 2020 05)))))

(deftest day06 ()
  (is (= 6809 (aoc2020.day06::part1 (input-for 2020 06))))
  (is (= 3394 (aoc2020.day06::part2 (input-for 2020 06)))))

(deftest day07 ()
  (is (= 332 (aoc2020.day07::part1 (input-for 2020 07))))
  (is (= 10875 (aoc2020.day07::part2 (input-for 2020 07)))))

(deftest day08 ()
  (is (= 1563 (aoc2020.day08::part1 (input-for 2020 08))))
  (is (= 767 (aoc2020.day08::part2 (input-for 2020 08)))))

(deftest day09 ()
  (is (= 26134589 (aoc2020.day09::part1 (input-for 2020 09))))
  (is (= 3535124 (aoc2020.day09::part2 (input-for 2020 09) 26134589))))

(deftest day10 ()
  (is (= 2760 (aoc2020.day10::part1 (input-for 2020 10))))
  (is (= 13816758796288 (aoc2020.day10::part2 (input-for 2020 10)))))

(deftest day11 ()
  (is (= 2359 (aoc2020.day11::part1 (input-for 2020 11))))
  (is (= 2131 (aoc2020.day11::part2 (input-for 2020 11)))))

(deftest day12 ()
  (is (= 439 (aoc2020.day12::part1 (input-for 2020 12))))
  (is (= 12385 (aoc2020.day12::part2 (input-for 2020 12)))))

(deftest day13 ()
  (is (= 246 (aoc2020.day13::part1 (input-for 2020 13))))
  (is (= 939490236001473 (aoc2020.day13::part2 (input-for 2020 13)))))

(deftest day14 ()
  (is (= 10717676595607 (aoc2020.day14::part1 (input-for 2020 14))))
  (is (= 3974538275659 (aoc2020.day14::part2 (input-for 2020 14)))))

(deftest day15 ()
  (is (= 1085 (aoc2020.day15::part1 (input-for 2020 15))))
  (is (= 10652 (aoc2020.day15::part2 (input-for 2020 15)))))

(deftest day16 ()
  (is (= 22977 (aoc2020.day16::part1 (input-for 2020 16))))
  (is (= 998358379943 (aoc2020.day16::part2 (input-for 2020 16)))))

(deftest day17 ()
  (is (= 247 (aoc2020.day17::part1 (input-for 2020 17))))
  (is (= 1392 (aoc2020.day17::part2 (input-for 2020 17)))))

(deftest day18 ()
  (is (= 75592527415659 (aoc2020.day18::part1 (input-for 2020 18))))
  (is (= 360029542265462 (aoc2020.day18::part2 (input-for 2020 18)))))

(deftest day19 ()
  (is (= 134 (aoc2020.day19::part1 (input-for 2020 19))))
  (is (= 377 (aoc2020.day19::part2 (input-for 2020 19)))))

(deftest day20 ()
  (is (= 5966506063747 (aoc2020.day20::part1 (input-for 2020 20))))
  (is (= 1714 (aoc2020.day20::part2 (input-for 2020 20)))))

(deftest day21 ()
  (is (= 2493 (aoc2020.day21::part1 (input-for 2020 21))))
  (is (string= "kqv,jxx,zzt,dklgl,pmvfzk,tsnkknk,qdlpbt,tlgrhdh" (aoc2020.day21::part2 (input-for 2020 21)))))

(deftest day22 ()
  (is (= 32629 (aoc2020.day22::part1 (input-for 2020 22))))
  (is (= 32519 (aoc2020.day22::part2 (input-for 2020 22)))))

(deftest day23 ()
  (is (string= "46978532" (aoc2020.day23::part1 (input-for 2020 23))))
  (is (= 163035127721 (aoc2020.day23::part2 (input-for 2020 23)))))

(deftest day24 ()
  (is (= 277 (aoc2020.day24::part1 (input-for 2020 24))))
  (is (= 3531 (aoc2020.day24::part2 (input-for 2020 24)))))
