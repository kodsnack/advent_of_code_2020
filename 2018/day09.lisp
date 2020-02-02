;;;; day09.lisp

(in-package #:aoc2018.day09)

(defun parse (input)
  (ppcre:register-groups-bind ((#'parse-integer players last-marble))
      ("(\\d+) players; last marble is worth (\\d+) points" input)
    (list players last-marble)))

(defun make-circle () (alexandria:circular-list 0))

(defun insert-after (x circle)
  (rplacd circle (cons x (cdr circle)))
  (cdr circle))

(defun remove-marble (circle)
  (prog1 (car circle)
    (rplaca circle (cadr circle))
    (rplacd circle (cddr circle))))

(defun add-score (players score)
  (incf (car players) score))

(defun winning-score (players)
  (loop for x on (cdr players)
        maximize (car x)
        until (eq x players)))

(defun make-players (n)
  (alexandria:make-circular-list n :initial-element 0))

(defun find-behind (from target n)
  (loop for x = (nthcdr n from) then (cdr x)
        for l on from
        until (eq x target)
        finally (return l)))

(defun play (players last-marble)
  (let* ((players (make-players players))
         (circle (make-circle))
         (last circle))
    (flet ((cw (n) (setf circle (nthcdr n circle)))
           (ccw (n)
             (setf circle (find-behind last circle n))
             (setf last circle))
           (insert (i) (setf circle (insert-after i circle))))
      (loop for player on players
            for i from 1 upto last-marble
            if (zerop (mod i 23)) do
              (ccw 7)
              (add-score player i)
              (add-score player (remove-marble circle))
            else do
              (cw 1)
              (insert i)
            finally
               (return (winning-score players))))))

(defun part1 (input)
  (apply #'play (parse input)))

(defun part2 (input)
  (destructuring-bind (players last-marble) (parse input)
    (play players (* 100 last-marble))))
