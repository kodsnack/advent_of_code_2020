;;;; day11.lisp

(in-package #:aoc2018.day11)

(defparameter *serial-number* 7989)

(defun hundreds-digit (n)
  (values (truncate (mod n 1000) 100)))

(defun power-level (x y)
  (let ((rack-id (+ x 10)))
    (- (hundreds-digit (* rack-id
                          (+ *serial-number*
                             (* rack-id y))))
       5)))

(defstruct point
  (base 0 :type fixnum)
  (horizontal 0 :type fixnum)
  (vertical 0 :type fixnum)
  (square 0 :type fixnum))

(defun make-grid (x-size y-size)
  (make-array (grid-dimensions (list x-size y-size)) :element-type 'point))

(defun grid-dimensions (dim) (mapcar #'1+ dim))
(defun grid-size (grid) (mapcar #'1- (array-dimensions grid)))

(defun set-levels (grid)
  (destructuring-bind (x-size y-size) (grid-size grid)
    (loop for x from 1 upto x-size do
      (loop for y from 1 upto y-size
            for l = (power-level x y)
            do (setf (aref grid x y) (make-point :base l :horizontal l :vertical l :square l)))))
  grid)

(defun update (grid x y)
  (let* ((point (aref grid x y))
         (base (point-base point))
         (horizontal (point-horizontal (aref grid (1+ x) y)))
         (vertical (point-vertical (aref grid x (1+ y))))
         (square (point-square (aref grid (1+ x) (1+ y)))))
    (setf (point-horizontal point) (+ base horizontal)
          (point-vertical point) (+ base vertical)
          (point-square point) (+ base horizontal vertical square))))

(defun grow (grid n)
  (destructuring-bind (x-size y-size) (grid-size grid)
    (let ((max 0)
          (mx 0)
          (my 0))
      (loop for y from 1 upto (- y-size n) do
        (loop for x from 1 upto (- x-size n)
              for s = (update grid x y)
              when (< max s)
                do (setf max s
                         mx x
                         my y)))
      (list mx my max))))

(defun find-max (grid break)
  (destructuring-bind (x-size y-size) (grid-size grid)
    (let ((max 0)
          (mx 0)
          (my 0)
          (ms 0))
      (loop for n from 1 below (min break x-size y-size)
            for (x y m) = (grow grid n)
            when (< max m)
              do (setf max m
                       mx x
                       my y
                       ms (1+ n)))
      (list mx my ms max))))

(defun part1 ()
  (find-max (set-levels (make-grid 300 300)) 3))

(defun part2 ()
  (find-max (set-levels (make-grid 300 300)) 300))
