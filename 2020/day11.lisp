;;;; day11.lisp

(in-package #:aoc2020.day11)

(defun next-line (line over under)
  (loop with next = (empty-like line)
        for ix from -2 upto (length next)
        for a = b and b = c and c = (dref over (1+ ix) #\.)
        and d = e and e = f and f = (dref line (1+ ix) #\.)
        and g = h and h = i and i = (dref under (1+ ix) #\.)
        when (array-in-bounds-p next ix)
          do (setf (aref next ix) (next-pos a b c d e f g h i))
        finally (return next)))

(defun empty-like (s) (make-string (length s) :initial-element #\.))

(defun dref (array i default)
  (if (and array (array-in-bounds-p array i))
      (aref array i)
      default))

(defun next-pos (a b c
                 d e f
                 g h i)
  (flet ((occupied (&rest args) (count #\# args)))
    (case e
      (#\. #\.)
      (#\# (if (>= (occupied a b c d f g h i) 4) #\L #\#))
      (#\L (if (= 0 (occupied a b c d f g h i)) #\# #\L)))))

(defun advance (lines)
  (loop for over = line
        and line = under
        and under in (append lines '(nil))
        when line collect (next-line line over under)))

(defun stabilize (lines advance)
  (loop repeat 10000
        for a = lines then b
        for b = (funcall advance a)
        when (equal a b)
          do (return a)))

(defun part1 (input)
  (reduce #'+ (mapcar (curry #'count #\#)
                      (stabilize (aoc:lines input) #'advance))))

(defun countv (string) (map 'vector (lambda (c) (if (eq c #\#) 1 0)) string))
(defun addv (a &rest r) (apply #'map 'vector #'+ a r))
(defun shift (n str)
  (loop with shifted = (empty-like str)
        for i below (length shifted)
        do (setf (char shifted i) (dref str (- i n) #\.))
        finally (return shifted)))

(defun combine (close far)
  (case close
    ((#\# #\L) close)
    (t far)))

(defun combine-views (close far)
  (map 'string #'combine close far))

(defun backwards-view (lines &optional (shift #'identity))
  (loop for line in lines
        and view = (empty-like (first lines))
              then (combine-views (funcall shift line)
                                  (funcall shift view))
        collect view))

(defun lateral-view (lines &optional (reverse #'identity))
  (mapcar
   (lambda (line)
     (loop for char across (funcall reverse line)
           and view = #\. then (combine char view)
           collect view into chars
           finally (return (funcall reverse (coerce chars 'string)))))
   lines))

(defun views (lines)
  (let ((reversed (reverse lines)))
    (list (backwards-view lines)
          (backwards-view lines (curry #'shift 1))
          (backwards-view lines (curry #'shift -1))
          (reverse (backwards-view reversed))
          (reverse (backwards-view reversed (curry #'shift 1)))
          (reverse (backwards-view reversed (curry #'shift -1)))
          (lateral-view lines)
          (lateral-view lines #'reverse))))

(defun count-views (views)
  (mapcar (curry #'mapcar #'countv) views))

(defun sum (counts)
  (apply #'mapcar #'addv counts))

(defun advance2 (lines)
  (loop with views = (sum (count-views (views lines)))
        for line in lines
        for view in views
        collect (next-line2 line view)))

(defun next-line2 (line view)
  (map 'string
       (lambda (c n)
         (case c
           (#\. #\.)
           (#\# (if (>= n 5) #\L #\#))
           (#\L (if (= n 0) #\# #\L))))
       line
       view))

(defun part2 (input)
  (reduce #'+ (mapcar (curry #'count #\#)
                      (stabilize (aoc:lines input) #'advance2))))


