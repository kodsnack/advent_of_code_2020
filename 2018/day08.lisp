;;;; day08.lisp

(in-package #:aoc2018.day08)

(defun parse (input)
  (read-from-string (format nil "(~a)" input)))

(defun make-node (children meta tail) (append (list children meta) tail))
(defun children (node) (car node))
(defun meta (node) (cadr node))
(defun tail (node) (cddr node))

(defun parse-node (data)
  (destructuring-bind (child-count meta-count &rest tail) data
    (multiple-value-bind (children tail) (parse-nodes child-count tail)
      (multiple-value-bind (meta tail) (parse-metas meta-count tail)
        (make-node children meta tail)))))

(defun parse-nodes (n data)
  (if (= n 0)
      (values nil data)
      (loop repeat n
            for node = (parse-node data) then (parse-node (tail node))
            collecting node into nodes
            finally (return (values nodes (tail node))))))

(defun parse-metas (n data)
  (loop for tail on data
        for meta in data
        collecting meta into metas
        repeat n
        finally (return (values metas tail))))

(defun sum-metas (node)
  (+ (apply #'+ (mapcar #'sum-metas (children node)))
     (apply #'+ (meta node))))

(defun part1 (input)
  (sum-metas (parse-node (parse input))))

(defun value (node)
  (if (children node)
      (apply #'+ (mapcar (lambda (i) (value (nth (1- i) (children node))))
                         (meta node)))
      (apply #'+ (meta node))))

(defun part2 (input)
  (value (parse-node (parse input))))
