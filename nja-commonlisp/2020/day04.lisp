;;;; day04.lisp

(in-package #:aoc2020.day04)

(defun read-passports (input)
  (mapcar #'read-fields (ppcre:split "\\n{2,}" input)))

(defparameter *fields* '(byr iyr eyr hgt hcl ecl pid cid))
(defparameter *required-fields* (remove 'cid *fields*))

(defun field-symbol (string)
  (find (string-upcase string) *fields*
        :test #'equal
        :key #'symbol-name))

(defun read-fields (fields-string)
  (loop for entry in (ppcre:split "\\s+" fields-string)
        for (key value) = (ppcre:split #\: entry)
        collect (cons (field-symbol key) value)))

(defun has-required-fields (passport)
  (every (lambda (f) (find f passport :key #'car)) *required-fields*))

(defun part1 (input)
  (count t (mapcar #'has-required-fields (read-passports input))))

(defun check-digits (s n)
  (and (= n (length s))
       (every #'digit-char-p s)))

(defun check-year (s min max)
  (and (check-digits s 4)
       (<= min (parse-integer s) max)))

(defun check-unit (s unit min max)
  (ppcre:register-groups-bind ((#'parse-integer d) u) ("([0-9]+)([a-z]+)" s)
    (and (string= unit u) (<= min d max))))

(defun check-eye-color (s)
  (member s '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") :test #'string=))

(defun check-hair-color (s)
  (ppcre:scan "#[0-9a-f]{6}" s))

(defun check-field (field)
  (destructuring-bind (key . value) field
   (case key
     ((byr) (check-year value 1920 2002))
     ((iyr) (check-year value 2010 2020))
     ((eyr) (check-year value 2020 2030))
     ((hgt) (or (check-unit value "cm" 150 193)
                (check-unit value "in" 59 76)))
     ((hcl) (check-hair-color value))
     ((ecl) (check-eye-color value))
     ((pid) (check-digits value 9))
     ((cid) t))))

(defun valid (passport)
  (and (has-required-fields passport)
       (every #'check-field passport)))

(defun part2 (input)
  (count t (mapcar #'valid (read-passports input))))
