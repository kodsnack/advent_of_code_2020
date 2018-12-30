;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf))

(defpackage #:aoc2018.day01 (:use #:cl))
(defpackage #:aoc2018.day02 (:use #:cl))
(defpackage #:aoc2018.day03 (:use #:cl))
(defpackage #:aoc2018.day04 (:use #:cl))
(defpackage #:aoc2018.day05 (:use #:cl))

(fiasco:define-test-package #:aoc2018.tests
  (:use #:aoc))
