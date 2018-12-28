;;;; package.lisp

(defpackage #:aoc
  (:use #:cl)
  (:export #:input-for #:lines #:trim-lf))

(defpackage #:aoc2018.day01 (:use #:cl))

(fiasco:define-test-package #:aoc2018.tests
  (:use #:aoc))
