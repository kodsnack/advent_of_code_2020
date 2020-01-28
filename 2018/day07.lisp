;;;; day07.lisp

(in-package #:aoc2018.day07)

(defun requirement (a b) (list a '-> b))
(defun anterior (req) (car req))
(defun posterior (req) (caddr req))
(defun names (req) (list (anterior req) (posterior req)))

(defun parse (line)
  (ppcre:register-groups-bind (((compose #'intern #'string-upcase) a b))
      ("Step (\\w) must be finished before step (\\w) can begin." line)
    (requirement a b)))

(defun node (c) (list c nil))
(defun name (node) (car node))
(defun anteriors (node) (cadr node))

(defun find-node (name nodes) (find name nodes :key #'name))
(defun add-anterior (node ante) (push ante (cadr node)))

(defun make-nodes (reqs)
  (let ((nodes (mapcar #'node (remove-duplicates (mapcan #'names reqs)))))
    (dolist (req reqs nodes)
      (add-anterior (find-node (posterior req) nodes)
                    (anterior req)))))

(defun name-sort (nodes) (sort nodes #'string< :key (compose #'symbol-name #'name)))
(defun free (nodes) (remove-if-not #'null nodes :key #'anteriors))
(defun next (nodes) (name-sort (free nodes)))
(defun clear (name node) (setf (cadr node) (delete name (cadr node))))

(defun determine-the-order (reqs)
  (loop for nodes = (make-nodes reqs) then (delete next nodes)
        for next = (first (next nodes))
        while next do (mapc (curry #'clear (name next)) nodes)
        collect (name next)))

(defun part1 (input)
  (format nil "~{~a~}" (determine-the-order (mapcar #'parse (aoc:lines input)))))

(defun seconds (name) (+ 60 (1+ (- (char-code (elt (symbol-name name) 0)) (char-code #\A)))))
(defun worker ()  (cons nil nil))
(defun begin (worker name) (rplaca worker name) (rplacd worker (seconds name)))
(defun work (worker) (when (seconds-left worker) (decf (cdr worker))))
(defun task (worker) (car worker))
(defun seconds-left (worker) (cdr worker))
(defun reset (worker) (rplaca worker nil) (rplacd worker nil))
(defun reap (workers)
  (loop for w in workers
        if (eq 0 (seconds-left w))
          collect (prog1 (task w)
                    (reset w))))
(defun ready (workers) (remove-if-not #'null workers :key #'task))
(defun in-progress (workers) (mapcar #'car workers))

(defun how-long-will-it-take (reqs)
  (loop with nodes = (make-nodes reqs)
        with target = (length nodes)
        with workers = (loop repeat 5 collect (worker))  ; 2 for test, 5 for prod
        with done
        for count from 0
        for x = (let ((completed (reap workers)))
                  (setf done (append done completed))
                  (dolist (c completed)
                    (mapcar (curry #'clear c) nodes)))
        for next = (mapcar #'name (next nodes))
        while (< (length done) target) do
          (mapc (lambda (w n)
                  (begin w n)
                  (setf nodes (remove n nodes :key #'name)))
                (ready workers)
                next)
          (mapc #'work workers)
        finally (return count)))

(defun part2 (input)
  (how-long-will-it-take (mapcar #'parse (aoc:lines input))))
