;;;; day08.lisp

(in-package #:aoc2020.day08)

(defparameter *ops* '(nop acc jmp))
(defun opsym (str) (find str *ops* :key #'symbol-name :test #'string-equal))

(defun op (ins) (car ins))
(defun (setf op) (new ins) (setf (car ins) new))
(defun arg (ins) (cdr ins))

(defun parse (line)
  (ppcre:register-groups-bind ((#'opsym op) (#'parse-integer arg))
      ("(acc|jmp|nop) ([+-]\\d+)" line)
    (cons op arg)))

(defun run (code)
  (loop with pc = 0
        with acc = 0
        for end = (= pc (length code))
        for ins = (when (< pc (length code)) (aref code pc))
        until (or end (null (op ins)))
        do (case (op ins)
             (nop (incf pc))
             (acc (incf acc (arg ins)) (incf pc))
             (jmp (incf pc (arg ins))))
           (setf (op ins) nil)
        finally (return (values acc end))))

(defun part1 (input) (run (apply #'vector (mapcar #'parse (aoc:lines input)))))

(defun patch (vector end)
  (let* ((ix (position-if (rcurry #'member '(nop jmp))
                          vector :key #'op :from-end t :end end))
         (ins (aref vector ix)))
    (setf (op ins) (if (eq 'nop (op ins)) 'jmp 'nop))
    ix))

(defun try-patches (code)
  (loop for vector = (apply #'vector (copy-tree code))
        for pos = (patch vector pos)
        for (acc end) = (multiple-value-list (run vector))
        until end
        finally (return (values acc pos))))

(defun part2 (input) (try-patches (mapcar #'parse (aoc:lines input))))
