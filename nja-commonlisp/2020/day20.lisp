;;;; day20.lisp

(in-package #:aoc2020.day20)

(defun parse-tile (str)
  (ppcre:register-groups-bind ((#'parse-integer id))
      ("Tile (\\d+):" str)
    (list id (read-bits 10 10 str))))

(defun read-bits (rows cols str)
  (loop with array = (make-array (list rows cols) :element-type 'bit)
        with i = 0
        for char across str
        for bit = (position char ".#")
        when bit
          do (setf (row-major-aref array i) bit)
             (incf i)
        finally (return array)))

(defun parse-tiles (input)
  (mapcar #'parse-tile (ppcre:split "\\n\\n" input)))

(defun like-array (array)
  (make-array (array-dimensions array)
              :element-type (array-element-type array)))

(defun rotate (array)
  (loop with rotated = (like-array array)
        with width = (array-dimension array 0)
        for row below width do
          (loop for col below (array-dimension array 1) do
            (setf (aref rotated col (- width row 1))
                  (aref array row col)))
        finally (return rotated)))

(defun transpose (array)
  (loop with transposed = (like-array array)
        for row below (array-dimension array 0) do
          (loop for col below (array-dimension array 1) do
            (setf (aref transposed col row)
                  (aref array row col)))
        finally (return transposed)))

(defun rotations (array)
  (loop for a = array then (rotate a)
        collect a repeat 4))

(defun orientations (array)
  (nconc (rotations array) (rotations (transpose array))))

(defun borders (array)
  (flet ((integer (bits) (reduce (lambda (a b) (+ (ash a 1) b)) bits)))
    (loop with size = (array-dimension array 0)
          for i below size
          collect (aref array 0 i) into top
          collect (aref array (1- size) i) into bottom
          collect (aref array i 0) into left
          collect (aref array i (1- size)) into right
          finally (return (list (cons 'top (integer top))
                                (cons 'bottom (integer bottom))
                                (cons 'left (integer left))
                                (cons 'right (integer right)))))))

(defun id (tile) (first tile))
(defun arr (tile) (second tile))

(defun index (tiles)
  (loop with index = (make-hash-table :test #'equal)
        for tile in tiles
        for (id array) = tile do
          (loop for orientation in (orientations array) do
            (loop for key in (borders orientation) do
              (push (list id orientation) (gethash key index))))
        finally (return index)))

(defun lengths (index)
  (loop for list being the hash-values in index
        collect (length list)))

(defun flip (dir)
  (cdr (assoc dir '((top . bottom)
                    (bottom . top)
                    (left . right)
                    (right . left)))))

(defun match (edge)
  (destructuring-bind (dir . n) edge
    (cons (flip dir) n)))

(defparameter *index* nil)

(defun fit (tile dir)
  (let ((edge (assoc dir (borders (arr tile)))))
    (find (id tile)
          (gethash (match edge) *index*)
          :key #'id
          :test (complement #'equal))))

(defun walk (tile dir)
  (loop for at = tile then next
        for next = (fit at dir)
        while next
        finally (return at)))

(defun corners (tile)
  (let* ((top-left (walk (walk tile 'top) 'left))
         (top-right (walk top-left 'right))
         (bottom-right (walk top-right 'bottom))
         (bottom-left (walk top-left 'bottom)))
    (list top-left top-right bottom-left bottom-right)))

(defun part1 (input)
  (let* ((tiles (parse-tiles input))
         (*index* (index tiles)))
    (reduce #'* (mapcar #'id (corners (first tiles))))))

(defun part2 (input)
  (let* ((tiles (parse-tiles input))
         (*index* (index tiles))
         (image (stitch-image tiles))
         (monsters (reduce #'max (mapcar #'count-patterns (orientations image)))))
    (- (bits image)
       (* (bits *pattern*) monsters))))

(defun bits (array)
  (loop for i below (array-total-size array)
        sum (row-major-aref array i)))

(defun tiling (tile)
  (loop for at = (walk tile 'bottom) then (fit at 'top)
        while at nconc (row at)))

(defun row (tile)
  (loop for at = (walk tile 'left) then (fit at 'right)
        while at
        collect at))

(defun stitch-image (tiles)
  (let* ((tilewidth (truncate (- (sqrt (array-total-size (arr (first tiles)))) 2)))
         (imagesize (truncate (* (sqrt (length tiles)) tilewidth)))
         (image (make-array (list imagesize imagesize) :element-type 'bit)))
    (loop for yoffset from 0 by tilewidth
          for rowtile = (walk (first tiles) 'top) then (fit rowtile 'bottom)
          while rowtile do
            (loop for tile in (row rowtile)
                  for xoffset from 0 by tilewidth do
                    (loop for y below tilewidth do
                      (loop for x below tilewidth do
                        (setf (aref image (+ yoffset y) (+ xoffset x))
                              (aref (arr tile) (1+ y) (1+ x))))))
          finally (return image))))

(defun print-image (image)
  (loop for i from 0 below (array-total-size image)
        for bit = (row-major-aref image i)
        when (zerop (mod i 96))
          do (terpri)
        do (princ (aref ".#" bit))))

(defun looker (pattern)
  (lambda (image row col)
    (loop for y below (array-dimension pattern 0)
          always (loop for x below (array-dimension pattern 1)
                       for bit = (aref pattern y x)
                       always (and (array-in-bounds-p image (+ row y) (+ col x))
                                   (or (zerop bit)
                                       (eql bit (aref image (+ row y) (+ col x)))))))))

(defun count-patterns (image)
  (let ((look (looker *pattern*)))
    (loop for y below (array-dimension image 0)
          sum (loop for x below (array-dimension image 1)
                    count (funcall look image y x)))))

(defparameter *pattern* (read-bits 3 20 (aoc:tr " " "." "
                  # 
#    ##    ##    ###
 #  #  #  #  #  #   ")))
