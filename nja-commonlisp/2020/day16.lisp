;;;; day16.lisp

(in-package #:aoc2020.day16)

(defun skip-past (x lines) (subseq lines (1+ (position x lines :test #'equal))))
(defun before (x lines) (subseq lines 0 (position x lines :test #'equal)))

(defparameter *fields*
  '(departure-location departure-station departure-platform departure-track
    departure-date departure-time arrival-location arrival-station
    arrival-platform arrival-track class duration price route row seat train type
    wagon zone))
(defun parse-numbers (line) (mapcar #'parse-integer (ppcre:split "," line)))
(defun fieldsym (name)
  (find (substitute #\- #\Space name)
        *fields*
        :key #'symbol-name
        :test #'string-equal))
(defun parse-rule (str)
  (destructuring-bind (name ranges) (ppcre:split ": " str)
    (cons (fieldsym name) (mapcar #'parse-range (ppcre:split " or " ranges)))))
(defun parse-range (str)
  (mapcar #'parse-integer (ppcre:split "-" str)))

(defun range-predicate (range)
  (destructuring-bind (x y) range
    (lambda (n) (<= x n y))))

(defun rule-predicate (rule)
  (or-predicates (mapcar #'range-predicate (rest rule))))

(defun or-predicates (predicates)
  (lambda (n)
    (some (lambda (p) (funcall p n)) predicates)))

(defun number-predicate (rules)
  (or-predicates (mapcar #'rule-predicate rules)))

(defun all-failing-in (p lists)
  (apply #'nconc (mapcar (curry #'remove-if p) lists)))

(defun rules (input)
  (mapcar #'parse-rule (before "" (aoc:lines input))))

(defun nearby-tickets (input)
  (mapcar #'parse-numbers
          (skip-past "nearby tickets:" (aoc:lines input))))

(defun part1 (input)
  (reduce #'+ (all-failing-in (number-predicate (rules input))
                              (nearby-tickets input))))

(defun ticket-predicate (rules)
  (let ((nump (number-predicate rules)))
    (curry #'every nump)))

(defun n-valid-fields (rules)
  (let ((predicates (mapcar #'rule-predicate rules)))
    (lambda (n)
      (remove nil
              (mapcar (lambda (p r) (when (funcall p n) (car r)))
                      predicates
                      rules)))))

(defun ticket-field-sets (rules)
  (let ((np (n-valid-fields rules)))
    (lambda (ticket)
      (mapcar np ticket))))

(defun reduce-fields (list-of-field-sets)
  (apply #'mapcar
         (lambda (&rest sets)
           (reduce #'intersection sets :initial-value *fields*))
         list-of-field-sets))

(defun singlep (x) (null (cdr x)))
(defun solved (fields) (flatten (remove-if-not #'singlep fields)))
(defun remove-solved (fields solved)
  (mapcar (lambda (f)
            (if (singlep f)
                f
                (set-difference f solved)))
          fields))

(defun solve (field-sets)
  (loop for f = field-sets then (remove-solved f (solved f))
        until (every #'singlep f)
        finally (return (flatten f))))

(defun my-ticket (input)
  (parse-numbers (first (skip-past "your ticket:" (aoc:lines input)))))

(defun field-values (p ticket fields)
  (mapcan (lambda (n f)
            (when (funcall p f) (list n)))
          ticket
          fields))

(defun departure-field-p (field)
  (str:starts-with? "departure" (symbol-name field) :ignore-case t))

(defun valid-tickets (tickets rules)
  (remove-if-not (ticket-predicate rules) tickets))

(defun part2 (input)
  (let* ((rules (rules input))
         (fields (solve (reduce-fields
                         (mapcar (ticket-field-sets rules)
                                 (valid-tickets (nearby-tickets input) rules))))))
    (reduce #'* (field-values #'departure-field-p (my-ticket input) fields))))
