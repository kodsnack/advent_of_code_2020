;;;; day09.lisp

(in-package #:aoc2020.day09)

(defparameter *n* 25)

(defun parse (input) (mapcar #'parse-integer (aoc:lines input)))

(defun sum-pairs (list)
  (mapcon (lambda (sublist)
            (mapcar (curry #'+ (car sublist)) (cdr sublist)))
          list))

(defun take (l n) (loop for x in l collect x repeat n))
(defun previous (list) (take (cdr list) *n*))

(defun make (l) (cons (first l) (sum-pairs (previous l))))
(defun numb (x) (car x))
(defun sums (x) (cdr x))
(defun valid? (x) (member (numb x) (sums x)))
(defun process (numbers) (nreverse (butlast (maplist #'make (reverse numbers)) *n*)))

(defun part1 (input)
  (numb (find-if-not #'valid? (process (parse input)))))

(defun ranger (target)
  (lambda (list)
    (loop for x in list
          sum x into sum
          collect x into range
          while (< sum target)
          finally (return (when (and (= sum target)
                                     (<= 2 (length range)))
                            range)))))

(defun sum-min-max (list)
  (+ (reduce #'min list) (reduce #'max list)))

(defun part2 (input target)
  (sum-min-max (first (remove nil (maplist (ranger target) (parse input))))))
