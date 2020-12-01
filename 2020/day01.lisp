;;;; day01.lisp

(in-package #:aoc2020.day01)

(defun entries (input) (mapcar #'parse-integer (aoc:lines input)))

(defun combine (f list)
  (mapcon (lambda (sublist)
            (mapcar (lambda (x)
                      (cons (car sublist) x))
                    (funcall f (cdr sublist))))
          list))
(defun pairs (list) (combine (curry #'mapcar #'list) list))
(defun get-sum-p (sum) (lambda (x) (= (apply #'+ x) sum)))

(defun part1 (input)
  (apply #'* (find-if (get-sum-p 2020) (pairs (entries input)))))

(defun triples (list) (combine #'pairs list))

(defun part2 (input)
  (apply #'* (find-if (get-sum-p 2020) (triples (entries input)))))
