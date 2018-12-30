;;;; day05.lisp

(in-package #:aoc2018.day05)

(defun reacts? (a b)
  (and a b (char/= a b) (char-equal a b)))

(defun react (input)
  (loop with past
        for unit across input
        if (reacts? unit (car past))
          do (pop past)
        else
          do (push unit past)
        finally (return (coerce (nreverse past) 'string))))

(defun part1 (input)
  (length (react (aoc:trim-lf input))))

(defun unit-types (input)
  (remove-duplicates (map 'string #'char-upcase input)))

(defun remove-unit (input unit)
  (remove-if (lambda (x) (char-equal unit x)) input))

(defun polymers-less-one-unit (input)
  (loop for unit across (unit-types input)
        collecting (remove-unit input unit)))

(defun part2 (input)
  (length (first (sort (mapcar #'react (polymers-less-one-unit (aoc:trim-lf input)))
                       #'< :key #'length))))
