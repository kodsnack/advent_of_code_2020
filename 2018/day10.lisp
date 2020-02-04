;;;; day10.lisp

(in-package #:aoc2018.day10)

(defun parse (line)
  (ppcre:register-groups-bind ((#'parse-integer x y dx dy))
      ("position=<( ?-?\\d+), ( ?-?\\d+)> velocity=<( ?-?\\d+), ( ?-?\\d+)>" line)
    (list x y dx dy)))

(defun move (x y dx dy)
  (list (+ x dx) (+ y dy) dx dy))

(defun tick (points)
  (mapcar (curry #'apply #'move) points))

(defun draw (points)
  (destructuring-bind (x-min y-min x-range y-range) (ranges points)
    (let ((array (make-array (list (1+ x-range) (1+ y-range))
                             :element-type 'standard-char :initial-element #\.)))
      (flet ((translate (x y &rest d) (list* (- x x-min) (- y y-min) d))
             (mark (array x y &rest d)
               (declare (ignore d))
               (setf (aref array x y) #\#)))
        (mapc (alexandria:compose
               (curry #'apply #'mark array)
               (curry #'apply #'translate))
              points)
        (with-output-to-string (s)
          (princ #\Newline s)
          (loop for y below (array-dimension array 1) do
            (format s "~&")
            (loop for x below (array-dimension array 0) do
              (format s "~a" (aref array x y)))))))))

(defun ranges (points)
  (loop for (x y) in points
        maximize x into x-max
        maximize y into y-max
        minimize x into x-min
        minimize y into y-min
        finally
           (return (list x-min
                         y-min
                         (abs (- x-min x-max))
                         (abs (- y-min y-max))))))

(defun minimize (points break)
  (loop for p = points then (tick p)
        for (x-min y-min x-range y-range) = (ranges p)
        for s from 0
        with min-y-range
        with min-p
        with min-s
        repeat break
        when (or (null min-y-range) (< y-range min-y-range))
          do (setf min-p p
                   min-y-range y-range
                   min-s s)
        finally (return (values min-p min-s))))

(defun part1&2 (input)
  (multiple-value-bind (points seconds)
      (minimize (mapcar #'parse (aoc:lines input)) 1e5)
    (values (draw points) seconds)))
