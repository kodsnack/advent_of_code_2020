;;;; day15.lisp

(in-package #:aoc2020.day15)

(defun init (numbers max)
  (let ((array (make-array max :element-type 'fixnum)))
    (loop for i fixnum from 1
          for n fixnum in numbers
          do (setf (aref array n) i)
          finally (return (the (simple-array fixnum) array)))))

(defun speak (numbers times)
  (declare (type fixnum times) (optimize (speed 3) (debug 0) (safety 0)))
  (let ((spoken (init numbers (1+ times)))
        (start (1+ (length numbers)))
        (last (car (last numbers))))
    (labels ((dist (n i) (if (eql (aref spoken n) 0)
                             0
                             (- i (aref spoken n) 1))))
      (loop for i fixnum from start to times
            for p fixnum = last then n
            for n fixnum = (dist p i)
            do (setf (aref spoken p) (1- i))
            finally (return n)))))

(defun parse (line) (mapcar #'parse-integer (ppcre:split "," line)))

(defun part1 (input)
  (speak (parse input) 2020))

(defun part2 (input)
  (speak (parse input) 30000000))
