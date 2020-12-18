;;;; day17.lisp

(in-package #:aoc2020.day17)

(defparameter *dimensions* 3)

(defun make-cube (lo &optional (hi 1)) (make-array (append (make-list (- *dimensions* 2) :initial-element hi)
                                                           (list lo lo))
                                                   :element-type 'bit))
(defun make-next (from) (make-cube (+ 2 (car (last (array-dimensions from))))
                                   (+ 2 (array-dimension from 0))))
(defun read-cube (input)
  (loop with cube = (make-cube (truncate (sqrt (length input))) 1)
        with i = 0
        for char across input
        for bit = (position char ".#")
        when bit
          do (setf (row-major-aref cube i) bit)
             (incf i)
        finally (return cube)))

(defun bitchar (bit) (schar ".#" bit))

(defun dref (cube &rest subscripts)
  (if (apply #'array-in-bounds-p cube subscripts)
      (apply #'aref cube subscripts)
      0))

(defun next (from)
  (let* ((next (make-next from))
         (positions (coords next)))
    (labels ((from (pos) (apply #'dref from (mapcar #'1- pos)))
             (offset (pos offset)
               (from (mapcar #'+ pos offset)))
             (neighbors (min max pos)
               (loop for ofs in *neighbors*
                     sum (offset pos ofs) into c
                     while (<= c max)
                     finally (return (<= min c max)))))
      (loop for pos in positions
            for f = (from pos)
            for n = (case f
                      (0 (if (neighbors 3 3 pos) 1 0))
                      (1 (if (neighbors 2 3 pos) 1 0)))
            do (setf (apply #'aref next pos) n))
      next)))

(defun count-active (cube)
  (loop for i below (array-total-size cube)
        sum (row-major-aref cube i)))

(defun cycles (from n)
  (loop for cube = from then (next cube)
        repeat n
        finally (return cube)))

(defun neighbors (d)
  (labels ((rec (d)
             (if (< 0 d)
                 (mapcan (lambda (cdr)
                           (mapcar (rcurry #'cons cdr) '(-1 0 1)))
                         (rec (1- d)))
                 '(nil))))
    (remove-if (curry #'every #'zerop) (rec d))))

(defun coords (cube)
  (labels ((rec (dimensions)
             (if (car dimensions)
                 (loop with d-1 = (rec (cdr dimensions))
                       for i below (car dimensions)
                       nconc (mapcar (curry #'cons i) d-1))
                 '(nil))))
    (rec (array-dimensions cube))))

(defparameter *neighbors* (neighbors 3))

(defun part1 (input)
  (count-active (cycles (read-cube input) 6)))

(defun part2 (input)
  (let ((*dimensions* 4)
        (*neighbors* (neighbors 4)))
    (count-active (cycles (read-cube input) 6))))
