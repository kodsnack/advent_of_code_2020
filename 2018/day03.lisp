;;;; day03.lisp

(in-package #:aoc2018.day03)

(defun parse (line)
  (ppcre:register-groups-bind (id x y w h)
      ("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" line)
    (mapcar #'parse-integer (list id x y w h))))

(defun id (claim) (first claim))
(defun x (claim) (second claim))
(defun y (claim) (third claim))
(defun width (claim) (fourth claim))
(defun height (claim) (fifth claim))

(defun max-size (claims)
  (loop for (id x y w h) in claims
        maximizing (+ x w) into width
        maximizing (+ y h) into height
        finally (return (list width height))))

(defun make-grid (claims)
  (make-array (max-size claims) :initial-element 0))

(defun mark (grid claim)
  (loop for x from (x claim)
        repeat (width claim)
        do (loop for y from (y claim)
                 repeat (height claim)
                 do (incf (aref grid x y)))))

(defun count-overlaps (grid)
  (loop for i below (array-dimension grid 0)
        summing (loop for j below (array-dimension grid 1)
                      counting (< 1 (aref grid i j)))))

(defun is-unique (grid claim)
  (loop for x from (x claim)
        repeat (width claim)
        always (loop for y from (y claim)
                     repeat (height claim)
                     always (= 1 (aref grid x y)))))

(defun find-unique (claims)
  (let ((grid (make-grid claims)))
    (dolist (claim claims)
      (mark grid claim))
    (find-if (lambda (c) (is-unique grid c)) claims)))

(defun part1 (input)
  (let* ((claims (mapcar #'parse (aoc:lines input)))
         (grid (make-grid claims)))
    (dolist (claim claims)
      (mark grid claim))
    (count-overlaps grid)))

(defun part2 (input)
  (id (find-unique (mapcar #'parse (aoc:lines input)))))
