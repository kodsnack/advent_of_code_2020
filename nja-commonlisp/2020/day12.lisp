;;;; day12.lisp

(in-package #:aoc2020.day12)

(defun action-sym (str) (find str '(N S E W L R F) :key #'symbol-name :test #'string-equal))

(defun parse (line)
  (ppcre:register-groups-bind ((#'action-sym action) (#'parse-integer magnitude))
      ("([NSEWLRF])(\\d+)" line)
    (cons action magnitude)))

(defun p (x y) (mapcar #'list (list x y 1)))
(defun x (p) (caar p))
(defun y (p) (caadr p))
(defun m (a b c d e f)
  (list (list a b c)
        (list d e f)
        (list 0 0 1)))

(defun scale (m x) (mul (m x 0 0 0 x 0) m))
(defun trans (m x y) (mul (m 1 0 x 0 1 y) m))
(defun dir (x)
  (case x
    (n (p  0  1))
    (s (p  0 -1))
    (e (p  1  0))
    (w (p -1  0))))
(defun rot (p mag)
  (mul (case mag
         (( 90 -270) (m  0 -1  0
                         1  0  0))
         ((180 -180) (m -1  0  0
                         0 -1  0))
         ((270  -90) (m  0  1  0
                        -1  0  0)))
       p))
(defun move (p q) (trans p (x q) (y q)))

(defun navigate (actions act)
  (loop for (action . magnitude) in actions
        for (pos head) = (multiple-value-list (funcall act action magnitude pos head))
        finally (return (values pos head))))

(defun act (action magnitude pos heading)
  (let ((pos (or pos (p 0 0)))
        (heading (or heading (p 1 0))))
    (values (case action
              ((n s e w) (move pos (scale (dir action) magnitude)))
              (f (move pos (scale heading magnitude)))
              (t pos))
            (case action
              (l (rot heading magnitude))
              (r (rot heading (- magnitude)))
              (t heading)))))

(defun distance (p) (+ (abs (x p)) (abs (y p))))

(defun mul (m n)
  (mapcar (lambda (row)
            (mapcar (lambda (col)
                      (reduce #'+ (mapcar #'* row col)))
                    (apply #'mapcar #'list n)))
          m))

(defun part1 (input)
  (distance (navigate (mapcar #'parse (aoc:lines input)) #'act)))

(defun act2 (action magnitude pos waypoint)
  (let ((pos (or pos (p 0 0)))
        (waypoint (or waypoint (p 10 1))))
    (values (case action
              (f (move pos (scale waypoint magnitude)))
              (t pos))
            (case action
              ((n s e w) (move waypoint (scale (dir action) magnitude)))
              (l (rot waypoint magnitude))
              (r (rot waypoint (- magnitude)))
              (t waypoint)))))

(defun part2 (input)
  (distance (navigate (mapcar #'parse (aoc:lines input)) #'act2)))
