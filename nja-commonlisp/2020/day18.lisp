;;;; day18.lisp

(in-package #:aoc2020.day18)

(defun read-homework (str)
  (read-from-string (format nil "(~a)" str)))

(defun eval-homework (x)
  (cond ((atom x) x)
        ((null (cdr x)) (car x))
        (t (destructuring-bind (a op b . rest) x
             (case op
               ((+ *) (eval-homework
                       (cons (funcall op
                                      (eval-homework a)
                                      (eval-homework b))
                             rest))))))))

(defun sum-homework (eval lines)
  (reduce #'+ (mapcar (compose eval #'read-homework) lines)))

(defun part1 (input)
  (sum-homework #'eval-homework (aoc:lines input)))

(defun eval-homework2 (x)
  (cond ((atom x) x)
        ((null (cdr x)) (eval-homework2 (car x)))
        (t (destructuring-bind (a op b . rest) x
             (case op
               (+ (eval-homework2 (cons (+ (eval-homework2 a)
                                           (eval-homework2 b))
                                        rest)))
               (* (* (eval-homework2 a)
                     (eval-homework2 (cons b rest)))))))))

(defun part2 (input)
  (sum-homework #'eval-homework2 (aoc:lines input)))
