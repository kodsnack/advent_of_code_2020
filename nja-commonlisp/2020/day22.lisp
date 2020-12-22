;;;; day22.lisp

(in-package #:aoc2020.day22)

(defun parse (input)
  (mapcar (compose #'player #'read-from-string (curry #'format nil "(~a)"))
          (cdr (ppcre:split "\\s*Player \\d:\\s*" input))))

(defun player (cards)
  (cons cards nil))

(defun add-cards (player &rest cards)
  (rplaca player (append (car player) cards)))

(defun draw-card (player)
  (pop (car player)))

(defun play-round (player1 player2)
  (let ((card1 (draw-card player1))
        (card2 (draw-card player2)))
    (if (> card1 card2)
        (add-cards player1 card1 card2)
        (add-cards player2 card2 card1))))

(defun winner (player1 player2)
  (cond ((null (car player1)) player2)
        ((null (car player2)) player1)))

(defun game (player1 player2)
  (loop until (winner player1 player2)
        do (play-round player1 player2)
          finally (return (winner player1 player2))))

(defun score (player)
  (loop for x downfrom (length (car player))
        for card in (car player)
        sum (* x card)))

(defun part1 (input)
  (score (apply #'game (parse input))))

(defun play-recursive-round (player1 player2)
  (let ((card1 (draw-card player1))
        (card2 (draw-card player2)))
    (case (cond ((and (<= card1 (length (car player1)))
                      (<= card2 (length (car player2))))
                 (let ((copy1 (copy player1 card1))
                       (copy2 (copy player2 card2)))
                   (if (eql copy1 (recursive-game copy1 copy2))
                       'player1
                       'player2)))
                ((> card1 card2) 'player1)
                (t 'player2))
      (player1 (add-cards player1 card1 card2))
      (player2 (add-cards player2 card2 card1)))))

(defun recursive-game (player1 player2)
  (let ((seen (make-hash-table :test 'equalp)))
    (flet ((seen ()
             (let ((key (cons (copy-list (car player1))
                              (copy-list (car player2)))))
               (cond ((gethash key seen) t)
                     (t (setf (gethash key seen) t)
                        nil)))))
      (loop for winner = (if (seen)
                             player1
                             (winner player1 player2))
            until winner
            do (play-recursive-round player1 player2)
            finally (return winner)))))

(defun copy (player n)
  (player (subseq (car player) 0 n)))

(defun part2 (input)
  (score (apply #'recursive-game (parse input))))
