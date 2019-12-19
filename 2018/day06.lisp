;;;; day06.lisp

(in-package #:aoc2018.day06)

(defun point (x y) (cons x y))
(defun x (point) (car point))
(defun y (point) (cdr point))

(defun parse (line)
  (ppcre:register-groups-bind ((#'parse-integer x y))
      ("(\\d+), (\\d+)" line)
    (point x y)))

(defun max-dimensions (points)
  (loop for (x . y) in points
        maximize x into max-x
        maximize y into max-y
        finally (return (list (1+ y) (1+ x)))))

(defun make-grid (points)
  (make-array (max-dimensions points) :initial-element nil))

(defun distance (a b)
  (+ (abs (- (x a) (x b)))
     (abs (- (y a) (y b)))))

(defun distances (point points)
  (sort (mapcar (lambda (x) (list (distance point x) x)) points)
        #'< :key #'car))

(defun closest (distances)
  (let ((a (first distances))
        (b (second distances)))
    (unless (= (car a) (car b)) (cadr a))))

(defun grid-points (grid)
  (loop for y below (array-dimension grid 0)
        nconc (loop for x below (array-dimension grid 1)
                    collect (point x y))))

(defun mark-point (grid point value)
  (setf (aref grid (y point) (x point)) value))

(defun mark-grid (grid points)
  (loop for point in (grid-points grid)
        do (mark-point grid point (closest (distances point points)))))

(defun read-grid (grid point)
  (aref grid (y point) (x point)))

(defun sum-grid (grid)
  (loop with sums = (make-hash-table :test #'equal)
        for point in (grid-points grid)
        for value = (read-grid grid point)
        when value
          do (incf (gethash value sums 0))
        finally (return sums)))

(defun by-sizes (sums)
  (sort (loop for point being the hash-keys in sums using (hash-value size)
              collect (list size point))
        #'> :key #'first))

(defun border-points (grid)
  (let ((rows (array-dimension grid 0))
        (cols (array-dimension grid 1)))
    (nconc (loop for x below cols collect (point x 0))
           (loop for y below rows collect (point 0 y))
           (loop for x from 1 below cols collect (point x (1- rows)))
           (loop for y from 1 below rows collect (point (1- cols) y)))))

(defun infinites (grid)
  (remove-duplicates (mapcar (lambda (p) (read-grid grid p))
                             (border-points grid))
                     :test #'equal))

(defun part1 (input)
  (let* ((points (mapcar #'parse (aoc:lines input)))
         (grid (make-grid points)))
    (mark-grid grid points)
    (let* ((sums (sum-grid grid))
           (infinites (infinites grid)))
      (dolist (infinite infinites)
        (remhash infinite sums))
      (car (first (by-sizes sums))))))

(defun distance-sum (point points)
  (loop for p in points summing (distance point p)))

(defun safe (point points)
  (< (distance-sum point points) 10000))

(defun part2 (input)
  (let ((points (mapcar #'parse (aoc:lines input))))
    (loop for x below 400
          sum (loop for y below 400
                    count (safe (point x y) points)))))
