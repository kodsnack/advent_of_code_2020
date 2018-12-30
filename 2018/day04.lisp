;;;; day04.lisp

(in-package #:aoc2018.day04)

(defun sorted-lines (input)
  (sort (aoc:lines input) #'string<))

(defun parse (line)
  (ppcre:register-groups-bind (date minute event id)
      ("\\[1518-(\\d\\d-\\d\\d) \\d\\d:(\\d\\d)] (Guard|falls asleep|wakes up) ?#?(\\d+)?" line)
    (setf event (alexandria:switch (event :test #'string=)
                  ("Guard" 'guard)
                  ("falls asleep" 'falls-asleep)
                  ("wakes up" 'wakes-up)))
    (if (eq event 'guard)
        (list event (parse-integer id))
        (list event date (parse-integer minute)))))

(defun event (entry) (first entry))
(defun id (entry) (second entry))
(defun minute (entry) (third entry))

(defun make-record (id)
  (list (make-array 60 :initial-element 'w) id))

(defun minutes (record) (first record))

(defun set-from (array idx x)
  (loop for i from idx below (array-dimension array 0)
        do (setf (aref array i) x)))

(defun process (entries)
  (loop with records
        for entry in entries
        if (eq (event entry) 'guard)
          do (push (make-record (id entry)) records)
        else
          do (set-from (minutes (car records)) (minute entry) (case (event entry)
                                                                (wakes-up 'w)
                                                                (falls-asleep 's)))
        finally (return (nreverse records))))

(defun slept-minutes (record)
  (loop for x across (minutes record)
        counting (eq x 's)))

(defun sum-slept-minutes (records)
  (loop with sums = (make-hash-table)
        for record in records
        do (incf (gethash (id record) sums 0) (slept-minutes record))
        finally (return sums)))

(defun find-max-sleeper (sums)
  (loop with max-id
        with max-minutes = 0
        for id being the hash-keys in sums using (hash-value minutes)
        when (< max-minutes minutes)
          do (setf max-minutes minutes
                   max-id id)
        finally (return (values max-id max-minutes))))

(defun select-guard-records (records id)
  (remove-if-not (lambda (r) (= (id r) id)) records))

(defun find-sleepiest-minute (minuteses)
  (flet ((sleep-count (minute)
           (loop for minutes in minuteses
                 counting (eq (aref minutes minute) 's))))
    (loop with max-minute
          with max = 0
          for minute below 60
          for count = (sleep-count minute)
          when (< max count)
            do (setf max-minute minute
                     max count)
          finally (return (values max-minute max)))))

(defun part1 (input)
  (let* ((records (process (mapcar #'parse (sorted-lines input))))
         (guard-id (find-max-sleeper (sum-slept-minutes records)))
         (guard-records (select-guard-records records guard-id))
         (minute (find-sleepiest-minute (mapcar #'minutes guard-records))))
    (* guard-id minute)))

(defun report (records guard-id)
  (multiple-value-bind (minute count) (find-sleepiest-minute
                                       (mapcar #'minutes (select-guard-records records guard-id)))
    (list guard-id minute count)))

(defun sort-reports (reports)
  (sort reports #'> :key #'third))

(defun part2 (input)
  (let* ((records (process (mapcar #'parse (sorted-lines input))))
         (guards (mapcar #'id records))
         (reports (mapcar (lambda (id) (report records id)) guards)))
    (destructuring-bind (id minute count) (first (sort-reports reports))
      (declare (ignore count))
      (* id minute))))
