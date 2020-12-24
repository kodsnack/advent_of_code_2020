;;;; day19.lisp

(in-package #:aoc2020.day19)

(defun section (input i)
  (elt (ppcre:split "\\n\\n" input) i))

(defun messages (input)
  (aoc:lines (section input 1)))

(defun rules (input)
  (mapcar #'parse-rule (aoc:lines (section input 0))))

(defun parse-rule (line)
  (destructuring-bind (id . rest) (parse-as-list (aoc:tr ":|" " ?" line))
    (cons id (if (member '? rest)
                 (alt (mapcar #'seq (split '? rest)))
                 (seq rest)))))

(defun parse-as-list (str)
  (let ((*package* (symbol-package '?)))
    (read-from-string (format nil "(~a)" str))))

(defun seq (rest) (cons :sequence rest))
(defun alt (rest) (cons :alternation rest))

(defun split (where list)
  (let ((pos (position where list)))
    (list (subseq list 0 pos)
          (subseq list (1+ pos)))))

(defun rule (rules i)
  (labels ((sub (r)
             (if (atom r)
                 r
                 (mapcar (compose #'sub (curry #'sublis rules)) r))))
   (sub (cdr (assoc i rules)))))

(defun checker (rule)
  (let ((scanner (ppcre:create-scanner rule)))
    (lambda (str)
      (multiple-value-bind (start end)
          (funcall scanner str 0 (length str))
        (and (eql 0 start) (eql (length str) end))))))

(defun part1 (input)
  (length (remove-if-not (checker (rule (rules input) 0))
                         (messages input))))

(defun patch (rules)
  (remove-if (rcurry #'member '(0 8 11)) rules :key #'car))

(defun rule0 (rules)
  (let ((scan42 (ppcre:create-scanner (rule rules 42)))
        (scan31 (ppcre:create-scanner (rule rules 31))))
    (lambda (str)
      (labels ((try (scanner from)
                 (multiple-value-bind (start end)
                     (funcall scanner str from (length str))
                   (when (eql start from) end)))
               (times (scanner from)
                 (loop for end = (try scanner (or end from))
                       and last = end
                       while end
                       count 1 into c
                       finally (return (values last c)))))
        (multiple-value-bind (s a) (times scan42 0)
          (when s
            (multiple-value-bind (e b) (times scan31 s)
              (and (eql e (length str))
                   (> a b)))))))))

(defun part2 (input)
  (length (remove-if-not (rule0 (patch (rules input)))
                         (messages input))))
