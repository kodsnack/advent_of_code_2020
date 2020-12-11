;;;; day07.lisp

(in-package #:aoc2020.day07)

(defvar *rules*)

(defun bag-symbol (string)
  (intern (string-upcase (aoc:tr " " "-" string)) :aoc2020.day07))

(defun color (thing) (car thing))
(defun contents (rule) (cdr rule))
(defun num (thing) (cdr thing))

(defun parse (line)
  (destructuring-bind (head rest) (ppcre:split " contain " line)
    (cons (ppcre:register-groups-bind ((#'bag-symbol bag))
              ("(\\w+ \\w+) bags" head)
            bag)
          (unless (string= rest "no other bags.")
            (mapcar (lambda (s)
                      (ppcre:register-groups-bind ((#'parse-integer n)
                                                   (#'bag-symbol bag))
                          ("(\\d+) (\\w+ \\w+) bags?" s)
                        (cons bag n)))
                    (ppcre:split ", " rest))))))

(defun all-colors () (mapcar #'color *rules*))
(defun outermost-colors () (set-difference (all-colors) (all-contained-colors)))
(defun all-contained-colors ()
  (remove-duplicates (mapcan #'contained-colors *rules*)))

(defun rule (color) (find color *rules* :key #'color))
(defun contained-colors (rule) (mapcar #'color (contents rule)))

(declaim (notinline contains-directly? contains-eventually? count-contained))

(defun contains-directly? (color-a color-b)
  (member color-b (contents (rule color-a)) :key #'color))

(defun contains-eventually? (color-a color-b)
  (or (contains-directly? color-a color-b)
      (some (lambda (a) (contains-eventually? a color-b))
            (contained-colors (rule color-a)))))

(defun count-contained (color)
  (reduce #'+
          (mapcar (lambda (c)
                    (destructuring-bind (color . n) c
                      (* n (1+ (count-contained color)))))
                  (contents (rule color)))))

(memoize 'contains-directly?)
(memoize 'contains-eventually?)
(memoize 'count-contained)

(defun part1 (input)
  (let ((*rules* (mapcar #'parse (aoc:lines input))))
    (length (remove-if-not
             (lambda (c) (contains-eventually? c 'shiny-gold))
             (all-colors)))))

(defun part2 (input)
  (let ((*rules* (mapcar #'parse (aoc:lines input))))
    (count-contained 'shiny-gold)))
