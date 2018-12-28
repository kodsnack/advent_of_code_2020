;;;; day01.lisp

(in-package #:aoc2018.day01)

(defun numbers (strings)
  (mapcar #'read-from-string strings))

(defun make-set ()
  (let ((set (make-hash-table)))
    (lambda (x)
      (prog1 (gethash x set)
        (setf (gethash x set) x)))))

(defun make-device ()
  (let ((frequencies (make-set))
        (freq 0))
    (funcall frequencies 0)
    (lambda (change)
      (funcall frequencies (incf freq change)))))

(defun calibrate (device changes)
  (loop for change in changes
        for freq = (funcall device change)
        until freq
        finally (return freq)))

(defun circular (list)
  (setf (cdr (last list)) list)
  list)

(defun part1 (input)
  (reduce #'+ (numbers (aoc:lines input))))

(defun part2 (input)
  (calibrate (make-device) (circular (numbers (aoc:lines input)))))
