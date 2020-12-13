;;;; day13.lisp

(in-package #:aoc2020.day13)

(defun wait (earliest bus)
  (- bus (rem earliest bus)))

(defun parse-nums (input)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" input)))

(defun part1 (input)
  (destructuring-bind (earliest . buses) (parse-nums input)
    (apply #'* (first (sort (mapcar (lambda (x) (list (wait earliest x) x))
                                    buses)
                            #'<
                            :key #'car)))))
(defun parse-ids (line)
  (mapcar (lambda (x) (parse-integer x :junk-allowed t))
          (ppcre:split "," line)))

(defun timestamp (ns)
  (if (null (cadr ns))
      (car ns)
      (let ((div (car ns))
            (first (1- (timestamp (cdr ns))))
            (step (reduce #'* (cdr ns))))
        (loop for i from first by step
              when (zerop (rem i div))
                do (return i)
              repeat div))))

(defun part2 (input)
  (timestamp (substitute 1 nil (parse-ids (car (last (aoc:lines input)))))))
