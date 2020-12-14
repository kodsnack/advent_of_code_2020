;;;; day14.lisp

(in-package #:aoc2020.day14)

(defun destsym (str)
  (find str '(mask mem) :test #'string-equal :key #'symbol-name))

(defun parser (parse-pattern)
  (lambda (line)
    (ppcre:register-groups-bind ((#'destsym action)) ("^(mask|mem)" line)
      (case action
        (mask
         (ppcre:register-groups-bind (pattern) ("mask = ([01X]+)" line)
           (cons 'mask (funcall parse-pattern pattern))))
        (mem
         (ppcre:register-groups-bind ((#'parse-integer addr val))
             ("mem\\[(\\d+)] = (\\d+)" line)
           (list 'mem addr val)))))))

(defun parse-mask-num (pattern)
  (list (num (aoc:tr "01X" "110" pattern))
        (num (aoc:tr "01X" "010" pattern))
        (num (aoc:tr "01X" "001" pattern))))

(defun num (bits) (parse-integer bits :radix 2))

(defun masker (mmask m nmask)
  (lambda (n)
    (logior (logand mmask m)
            (logand nmask n))))

(defun run (code)
  (loop with mem = (list)
        and mask = #'identity
        for (f a . r) in code 
        do (case f
             (mask (setf mask (apply #'masker a r)))
             (mem (setf (getf mem a) (apply mask r))))
        finally (return mem)))

(defun memsum (mem) (loop for (addr val) on mem by #'cddr sum val))

(defun part1 (input)
  (memsum (run (mapcar (parser #'parse-mask-num) (aoc:lines input)))))

(defun expand (pattern &optional ci char)
  (setf pattern (copy-seq pattern))
  (when char (setf (char pattern ci) char))
  (loop for i from (or ci 0) below (length pattern)
        when (char= (char pattern i) #\X)
          do (return (nconc (expand pattern i #\0)
                            (expand pattern i #\1)))
        finally (return (list pattern))))

(defun floater (pattern)
  (lambda (addr)
    (setf addr (copy-seq addr))
    (loop for i below (length addr)
          for m = (char pattern i)
          do (case m ((#\1 #\X) (setf (char addr i) m)))
          finally (return (expand addr)))))

(defun run2 (code)
  (loop with mem = (make-hash-table)
        and float = (floater "000000000000000000000000000000000000")
        for (f a v) in code
        do (case f
             (mask (setf float (funcall #'floater a)))
             (mem (dolist (addr (funcall float (bits a)))
                    (setf (gethash (num addr) mem) v))))
        finally (return mem)))

(defun bits (num) (format nil "~36,'0b" num))

(defun hashsum (mem) (loop for v being the hash-values in mem sum v))

(defun part2 (input)
  (hashsum (run2 (mapcar (parser #'list) (aoc:lines input)))))
