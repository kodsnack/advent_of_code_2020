;;;; day24.lisp

(in-package #:aoc2020.day24)

(defparameter *directions*
  '((nw (-1  0))
    (ne (-1  1))
    (w  ( 0 -1))
    (e  ( 0  1))
    (sw ( 1 -1))
    (se ( 1  0))))

(defun dirsym (&rest x)
  (car (find (coerce x 'string) *directions* :key #'car :test #'string-equal)))

(defun parse-directions (line)
  (with-input-from-string (s line)
    (loop for dir = (when-let ((c (read-char s nil)))
                      (case c
                        ((#\w #\e) (dirsym c))
                        ((#\n #\s) (dirsym c (read-char s)))))
          while dir
          collect dir)))

(defun pos (dir)
  (cadr (assoc dir *directions*)))

(defun add (a b)
  (mapcar #'+ a b))

(defun walk (directions)
  (reduce #'add (mapcar #'pos directions)))

(defun flip-tile (floor pos)
  (flet ((flip (color) (case color (white 'black) (black 'white))))
    (setf (gethash pos floor) (flip (gethash pos floor 'white)))))

(defun count-black-tiles (floor)
  (loop for tile being the hash-values in floor
        count (eql 'black tile)))

(defun init-floor (directions)
  (let ((floor (make-hash-table :test #'equal)))
    (dolist (dir directions floor)
      (flip-tile floor (walk dir)))))

(defun part1 (input)
  (count-black-tiles (init-floor (mapcar #'parse-directions (aoc:lines input)))))

(defun map-adjacant (f pos)
  (map nil (lambda (d) (funcall f (add pos (cadr d))))
       *directions*))

(defun exec (floor)
  (let ((new (make-hash-table :test #'equal)))
    (labels ((black-adjacant (pos)
               (loop for (nil adj) in *directions*
                     for tile = (gethash (add adj pos) floor)
                     count (eql tile 'black) into count
                     while (< count 3)
                     finally (return count)))
             (rule (pos &optional tile)
               (unless (gethash pos new)
                 (let ((count (black-adjacant pos)))
                   (setf (gethash pos new)
                         (case (or tile (gethash pos floor 'white))
                           (black (if (or (zerop count) (> count 2))
                                      'white
                                      'black))
                           (white (if (= count 2)
                                      'black
                                      'white))))))))
      (loop for pos being the hash-keys in floor
              using (hash-value tile)
            when (eql 'black tile)
            do (rule pos tile)
               (map-adjacant #'rule pos)
            finally (return new)))))

(defun days (n start)
  (loop for i upto n
        for floor = start then (exec floor)
        finally (return floor)))

(defun part2 (input)
  (count-black-tiles
   (days 100 (init-floor (mapcar #'parse-directions (aoc:lines input))))))
