;;;; day12.lisp

(in-package #:aoc2018.day12)

(defun tr (old new seq)
  (loop for o across old
        for n across new
        for s = (substitute n o (or s seq))
        finally (return s)))

(defun tr-binary (str) (tr ".#" "01" str))
(defun parse-binary (str) (parse-integer str :radix 2))

(defun parse-rules (input)
  (let ((rules (make-array 32 :element-type 'bit)))
    (ppcre:do-register-groups ((#'parse-binary idx val))
        ("([01]{5}) => ([01])" (tr-binary input) rules)
      (setf (bit rules idx) (the bit val)))))

(defstruct (state (:constructor blank-state (offset length &aux (plants (make-array length :element-type 'bit)))))
  (offset 0 :type integer)
  (plants (make-array 0 :element-type 'bit) :type simple-bit-vector))

(defun get-pos (state idx)
  (+ idx (state-offset state)))
(defun get-idx (state pos)
  (- pos (state-offset state)))
(defun set-plant (state pos &optional (bit 1))
  (setf (bit (state-plants state) (get-idx state pos)) bit))
(defun get-plant (state pos)
  (bit (state-plants state) (get-idx state pos)))
(defun in-bounds-p (state pos)
  (array-in-bounds-p (state-plants state) (get-idx state pos)))
(defun end-pos (state)
  (get-pos state (length (state-plants state))))

(defun parse-state (input)
  (ppcre:register-groups-bind (digits)
      ("^initial state: ([01]+)" (tr-binary input))
    (loop with state = (blank-state 0 (length digits))
          for digit across digits
          for pos from 0
          do (set-plant state pos (ecase digit (#\0 0) (#\1 1)))
          finally (return state))))

(defun prepare-next-state (state)
  (let* ((first-index (or (position 1 (state-plants state)) 0))
         (first-pos (- (get-pos state first-index) 2))
         (last-index ( or (position 1 (state-plants state) :from-end t) 0))
         (last-pos (+ (get-pos state last-index) 3))
         (length (- last-pos first-pos)))
    (blank-state first-pos length)))

(defun shift-in (idx bit)
  (the (unsigned-byte 5) (logand #b11111 (+ (the bit bit) (ash idx 1)))))

(defun advance (state rules)
  (loop with next = (prepare-next-state state)
        for sp from (state-offset next)
        for dp from (- sp 2) below (end-pos next)
        for bit = (if (in-bounds-p state sp)
                      (get-plant state sp)
                      0)
        for idx = (shift-in (or idx 0) bit)
        for val = (bit rules idx)
        do (if (in-bounds-p next dp)
               (set-plant next dp val)
               (assert (zerop val)))
        finally (return next)))

(defun generations (state rules generations)
  (loop for s = state then (advance s rules)
        repeat generations
        finally (return s)))

(defun count-plants (state)
  (loop for bit across (state-plants state)
        for x from (state-offset state)
        summing (* bit x)))

(defun part1 (input)
  (let ((state (parse-state input))
        (rules (parse-rules input)))
    (count-plants (generations state rules 20))))

(defun stabilize (state rules)
  (loop for p = state then s
        for s = (advance state rules) then (advance s rules)
        for i from 0
        until (equal (state-plants p) (state-plants s))
        finally (return (values p i))))

(defun part2 (input)
  (let ((state (parse-state input))
        (rules (parse-rules input)))
    (multiple-value-bind (stable generations) (stabilize state rules)
      (incf (state-offset stable) (- 50000000000 generations))
      (count-plants stable))))
