;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf #:strip-cr))

(defpackage #:aoc2020.day01 (:use #:cl) (:import-from #:alexandria #:curry))
(defpackage #:aoc2020.day02 (:use #:cl))
(defpackage #:aoc2020.day03 (:use #:cl))

(fiasco:define-test-package #:aoc2020.tests
  (:use #:aoc))
