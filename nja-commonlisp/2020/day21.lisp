;;;; day21.lisp

(in-package #:aoc2020.day21)

(defun parse-line (line)
  (ppcre:register-groups-bind (ingredients allergens)
      ("(.*) \\(contains (.*)\\)" line)
    (list (read-as-list ingredients)
          (read-as-list (aoc:tr "," " " allergens)))))

(defun read-as-list (str)
  (let ((*package* (symbol-package 'read-as-list)))
    (read-from-string (format nil "(~a)" str))))

(defparameter *data* nil)
(defun ingredients () (remove-duplicates (reduce #'append (mapcar #'first *data*))))
(defun allergens () (remove-duplicates (reduce #'append (mapcar #'second *data*))))

(defun state (ingredients allergens)
  (mapcar (rcurry #'cons ingredients) allergens))

(defun add (state datum)
  (destructuring-bind (ingredients allergens) datum
    (dolist (allergen allergens state)
      (let ((allergen-state (assoc allergen state)))
        (setf (cdr allergen-state)
              (intersection (cdr allergen-state) ingredients))))))

(defun process (state)
  (dolist (datum *data* state)
    (add state datum)))

(defun allergenic-ingredients (state)
  (remove-duplicates (reduce #'append (mapcar #'cdr state))))

(defun safe-ingredients (state)
  (set-difference (ingredients) (allergenic-ingredients state)))

(defun count-ingredients (counted)
  (loop for (ingredients) in *data*
        sum (reduce #'+ (mapcar (rcurry #'count ingredients) counted))))

(defun part1 (input)
  (let ((*data* (mapcar #'parse-line (aoc:lines input))))
    (count-ingredients (safe-ingredients
                        (process (state (ingredients)
                                        (allergens)))))))

(defun allergen-of (state ingredient)
  (car (find-if (curry #'member ingredient) state :key #'cdr)))

(defun process2 (state)
  (let ((allergens (mapcar #'car state))
        (ingredient-sets (mapcar #'cdr state)))
    (mapcar #'cons allergens (solve ingredient-sets))))

(defun singlep (x) (null (cdr x)))
(defun solved (fields) (flatten (remove-if-not #'singlep fields)))
(defun remove-solved (fields solved)
  (mapcar (lambda (f)
            (if (singlep f)
                f
                (set-difference f solved)))
          fields))

(defun solve (field-sets)
  (loop for f = field-sets then (remove-solved f (solved f))
        until (every #'singlep f)
        finally (return f)))

(defun canonical-dangerous-ingredient-list (solved-state)
  (sort (allergenic-ingredients solved-state)
        #'string<
        :key (curry #'allergen-of solved-state)))

(defun part2 (input)
  (let ((*data* (mapcar #'parse-line (aoc:lines input))))
    (format nil "~{~(~a~)~^,~}"
            (canonical-dangerous-ingredient-list
             (process2 (process (state (ingredients) (allergens))))))))
