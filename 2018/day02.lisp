;;;; day02.lisp

(in-package #:aoc2018.day02)

(defun make-char-count ()
  (make-hash-table))

(defun add-count (count x)
  (or (gethash x count) (setf (gethash x count) 0))
  (incf (gethash x count)))

(defun has-count (count x)
  (loop for v being the hash-values in count
          thereis (= x v)))

(defun letter-count (string)
  (loop with count = (make-char-count)
        for ch across string
        do (add-count count ch)
        finally (return count)))

(defun checksum (ids)
  (let ((counts (mapcar #'letter-count ids)))
    (* (count-if (lambda (c) (has-count c 2)) counts)
       (count-if (lambda (c) (has-count c 3)) counts))))

(defun make-string-count ()
  (make-hash-table :test 'equal))

(defun string-count (strings)
  (loop with count = (make-string-count)
        for string in strings
        do (add-count count string)
        finally (return count)))

(defun find-string-duplicate (strings)
  (loop with count = (string-count strings)
        for string being the hash-keys in count using (hash-value n)
        when (< 1 n)
          do (return string)))

(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun strip-character (i string)
  (let ((copy (make-array (length string)
                          :fill-pointer 0
                          :adjustable t
                          :element-type (array-element-type string))))
    (loop for j below i
          do (vector-push (aref string j) copy))
    (loop for j from (1+ i) below (length string)
          do (vector-push (aref string j) copy))
    copy))

(defun find-common-characters (ids)
  (loop for i below (length (car ids))
        for match = (find-string-duplicate (mapcar (alexandria:curry #'strip-character i) ids))
        until match
        finally (return match)))

(defun part1 (input)
  (checksum (aoc:lines input)))

(defun part2 (input)
  (find-common-characters (aoc:lines input)))
