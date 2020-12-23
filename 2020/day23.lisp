;;;; day23.lisp

(in-package #:aoc2020.day23)

(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defun parse (input)
  (map 'list #'digit-char-p input))

(defun cups (head)
  (let ((tail (loop for i fixnum
                    from (1+ (the fixnum (reduce #'max head))) upto *n*
                    collect i)))
    (nconc head tail head)))

(defparameter *n* 9)
(defparameter *index* nil)
(declaim (type fixnum *n*)
         (type simple-vector index))

(defun locate (label)
  (declare (type fixnum label))
  (the list (aref *index* label)))

(defun index (cups)
  (loop with index = (make-array (1+ *n*)
                                 :initial-element nil
                                 :element-type 'list)
        for cons on cups
        for label in cups
        until (aref index label)
        do (setf (aref index label) cons)
        finally (return index)))

(defun pick (cups)
  (let ((pick (cdr cups)))
    (rplacd cups (cddddr cups))
    (rplacd (cddr pick) nil)
    pick))

(defun move (cups)
  (let ((pick (pick cups)))
    (place (destination cups pick) pick)
    (cdr cups)))

(defun moves (n start)
  (declare (type fixnum n))
  (loop for cups = start then (move cups)
        repeat n))

(defun destination (cups pick)
  (loop for d fixnum = (minus-one (car cups)) then (minus-one d)
        while (member d pick)
        finally (return (locate d))))

(defun minus-one (label)
  (declare (type fixnum label))
  (1+ (mod (- label 2) *n*)))

(defun place (destination pick)
  (rplacd (cddr pick) (cdr destination))
  (rplacd destination pick))

(defun take (n list)
  (declare (type fixnum n))
  (loop for x fixnum in list collect x repeat n))

(defun part1 (input)
  (let* ((*n* 9)
         (cups (cups (parse input)))
         (*index* (index cups)))
    (moves 100 cups)
    (format nil "~{~d~}" (take 8 (cdr (locate 1))))))

(defun part2 (input)
  (let* ((*n* 1000000)
         (cups (cups (parse input)))
         (*index* (index cups)))
    (moves 10000000 cups)
    (reduce #'* (take 2 (cdr (locate 1))))))
