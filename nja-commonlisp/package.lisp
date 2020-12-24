;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf #:strip-cr #:tr))

(defpackage #:aoc2020.day01 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2020.day02 (:use #:cl))
(defpackage #:aoc2020.day03 (:use #:cl))
(defpackage #:aoc2020.day04 (:use #:cl))
(defpackage #:aoc2020.day05 (:use #:cl))
(defpackage #:aoc2020.day06 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day07 (:use #:cl #:fare-memoization))
(defpackage #:aoc2020.day08 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day09 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day10 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day11 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day12 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day13 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day14 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day15 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day16 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day17 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day18 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day19 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day20 (:use #:cl))
(defpackage #:aoc2020.day21 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day22 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day23 (:use #:cl #:alexandria))
(defpackage #:aoc2020.day24 (:use #:cl #:alexandria))

(fiasco:define-test-package #:aoc2020.tests
  (:use #:aoc))
