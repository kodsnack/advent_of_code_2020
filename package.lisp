;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf #:strip-cr))

(defpackage #:aoc2018.day01 (:use #:cl))
(defpackage #:aoc2018.day02 (:use #:cl))
(defpackage #:aoc2018.day03 (:use #:cl))
(defpackage #:aoc2018.day04 (:use #:cl))
(defpackage #:aoc2018.day05 (:use #:cl))
(defpackage #:aoc2018.day06 (:use #:cl))
(defpackage #:aoc2018.day07 (:use #:cl) (:import-from #:alexandria #:compose #:curry))
(defpackage #:aoc2018.day08 (:use #:cl))
(defpackage #:aoc2018.day09 (:use #:cl))
(defpackage #:aoc2018.day10 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2018.day11 (:use #:cl))

(fiasco:define-test-package #:aoc2018.tests
  (:use #:aoc))
