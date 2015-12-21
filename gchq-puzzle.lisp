;;;; gchq-puzzle.lisp
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:gchq-puzzle)

(defparameter *columns* '((7 2 1 1 7)
                          (1 1 2 2 1 1)
                          (1 3 1 3 1 3 1 3 1)
                          (1 3 1 1 5 1 3 1)
                          (1 3 1 1 4 1 3 1)
                          (1 1 1 2 1 1)
                          (7 1 1 1 1 1 7)
                          (1 1 3)
                          (2 1 2 1 8 2 1)
                          (2 2 1 2 1 1 1 2)
                          (1 7 3 2 1)
                          (1 2 3 1 1 1 1 1)
                          (4 1 1 2 6)
                          (3 3 1 1 1 3 1)
                          (1 2 5 2 2)
                          (2 2 1 1 1 1 1 1 2 1)
                          (1 3 3 2 1 8 1)
                          (6 2 1)
                          (7 1 4 1 1 3)
                          (1 1 1 1 4)
                          (1 3 1 3 7 1)
                          (1 3 1 1 1 2 1 1 4)
                          (1 3 1 4 3 3)
                          (1 1 2 2 6 1)
                          (7 1 3 2 1 1)))
(defparameter *rows* '((7 3 1 1 7)
                       (1 1 2 2 1 1)
                       (1 3 1 3 1 1 3 1)
                       (1 3 1 1 6 1 3 1)
                       (1 3 1 5 2 1 3 1)
                       (1 1 2 1 1)
                       (7 1 1 1 1 1 7)
                       (3 3)
                       (1 2 3 1 1 3 1 1 2)
                       (1 1 3 2 1 1)
                       (4 1 4 2 1 2)
                       (1 1 1 1 1 4 1 3)
                       (2 1 1 1 2 5)
                       (3 2 2 6 3 1)
                       (1 9 1 1 2 1)
                       (2 1 2 2 3 1)
                       (3 1 1 1 1 5 1)
                       (1 2 2 5)
                       (7 1 2 1 1 1 3)
                       (1 1 2 1 2 2 1)
                       (1 3 1 4 5 1)
                       (1 3 1 3 10 2)
                       (1 3 1 1 6 6)
                       (1 1 2 1 1 2)
                       (7 2 1 2 5)))

(defun state-at (rows i j)
  "Figure out whether the cell at i,j is on or off."
  (let ((cur-row (aref rows j)))
    (loop 
       for cur-count = i then (- cur-count cell-count)
       for cell-count across cur-row
       for idx from 0
       until (< cur-count 0)
       finally (return (= 0 (mod idx 2))))))

(defun to-png (file-name rows &optional (square-size 20))
  "Write puzzle state to a .png file."
  (let ((psize (array-dimension rows 0)))
    (cl-cairo2:with-png-file
        (file-name :argb32 (* square-size psize) (* square-size psize))
      (cl-cairo2:set-source-rgba 1.0 1.0 1.0 1.0)
      (cl-cairo2:paint)
      (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
      (dotimes (a psize)
        (dotimes (b psize)
          (when (state-at rows a b)
            (cl-cairo2:rectangle (* a square-size) (* b square-size) square-size square-size)
            (cl-cairo2:fill-path)))))))

(defun calculate-full-row-states (numbers)
  "numbers is a list of lists where each inner list element is the length of contiguos black cells.
   This function creates an array of arrays that contains the data in numbers, but adds in contiguos white cell lengths.
   The result is an array of arrays where the elements at even indices are the length of contiguos white cells and 
   the elements at odd indices are the length of contiguos black cells from the input list."
  (make-array (length numbers)
              :initial-contents
              (mapcar 
               (lambda (row) 
                 (let ((temp (make-array (* 2 (+ 1 (length row))) :initial-element 0))
                       (total 0))
                   (loop
                      for val in row
                      for i from 0
                      do
                        (setf (aref temp (+ 1 (* i 2))) val)
                        (setf (aref temp (* i 2)) 1)
                        (incf total (+ val 1)))
                   (setf (aref temp 0) 0)
                   (decf total)
                   (setf (aref temp (* 2 (length row))) (- (length numbers) total))
                   temp))
               numbers)))

(defun calculate-column-extents (rows column)
  "Calculate the lengths of contiguous black cells in a column.  Returns a list of lengths."
  (let ((psize (array-dimension rows 0))
        (cur-col nil)
        (cur-count 0))
    (loop
       for j below psize
       do
         (if (state-at rows column j)
             (incf cur-count)
             (progn
               (if (/= cur-count 0)
                   (push cur-count cur-col))
               (setf cur-count 0))))
    (if (/= cur-count 0)
        (push cur-count cur-col))
    (reverse cur-col)))

(defun all-column-extents (rows)
  (loop for i below (array-dimension rows 0)
       collecting (list (calculate-column-extents rows i))))

(defun inner-solve (rows ri ii goal)
  (let* ((slack-idx (- (array-dimension (aref rows ri) 0) 2))
         (slack-value (aref (aref rows ri) slack-idx)))

    (cond ((equal (all-column-extents rows) goal)
           (return-from inner-solve t))

          ((> slack-value 0)
           (incf (aref (aref rows ri) ii))
           (decf (aref (aref rows ri) slack-idx))
           (if (inner-solve rows ri ii goal)
               (return-from inner-solve t))
           
           (if (< (+ 1 ri) (array-dimension rows 0))
               (if (inner-solve rows (+ 1 ri) ii goal)
                   (return-from inner-solve t))
               (progn
                 (decf (aref (aref rows ri) ii))
                 (incf (aref (aref rows ri) slack-idx))
                 (if (< (+ 2 ii) slack-idx)
                     (return-from inner-solve (inner-solve rows ri (+ 2 ii) goal))
                     (return-from inner-solve nil)))))
          (t
           (if (< (+ 1 ri) (array-dimension rows 0))
               (inner-solve rows (+ 1 ri) ii goal)
               nil)))))
          
          ;; (if (> (aref (aref rows ri) slack-idx) 0)
          ;;         (progn
                    
          ;;           (let ((solved ))
          ;;             (if solved
          ;;                 t
          ;;                 (progn 
          ;;                   (incf (aref (aref rows ri) slack-idx))
          ;;                   nil))))))
          ;; (progn
          ;;   (decf (aref (aref rows ri) ii))
          ;;   (if (and (< (+ 1 ri) (array-dimension rows 0)) (inner-solve rows (+ 1 ri) ii goal))
          ;;       t
          ;;       (if (< (+ 1 ii) (array-dimension (aref rows ri) 0))
          ;;           (inner-solve rows ri (+ 1 ii) goal)
          ;;           nil))))))
      
      

;; (defun inner-solve (rows r c goal)
;;   ;; (format t "Inner solve: ~a ~a ~a ~a~%" rows r c goal)
;;   (loop for idx-outer below (array-dimension rows 0)
;;      do
;;        (loop for idx-inner below (array-dimension (aref rows idx-outer) 0) by 2
;;           do
            
       
;;        (if (aref row c)
       
;;   (let* (
;;          (cur-row-size (array-dimension (aref rows r) 0))
;;          (cur-row-slack (aref (aref rows r) (- cur-row-size 2))))
;;     (if (> cur-row-slack 0)
;;         (progn
;;           (incf (aref (aref rows r) c))
;;           (decf (aref (aref rows r) (- cur-row-size 2)))
;;           (if (equal (all-column-extents rows) goal)
;;               (return-from inner-solve t)
;;               (if (inner-solve rows r c goal)
;;                   (return-from inner-solve t)
;;                   (if (inner-solve rows (+ 2 r) c goal)
;;                       (return-from inner-solve t)
;;                       (inner-solve rows (+ 2 r) c goal)
;;                       (progn
;;                         (decf (aref (aref rows r) c))
;;                         (incf (aref (aref rows r) (- cur-row-size 2)))
;;                         (if (< (+ 2 r) (array-dimension (aref rows r) 0))
;;                             (inner-solve rows (+ 2 r) c goal)
;;                             nil)))))))
;;         (if (cdr goal)
;;             (inner-solve rows r  c (cdr goal))
;;             nil))))


;; (defun solve-column (rows column goal)
;;   "Modify the rows structure so that the black cell extents of the specified column matches the goal."
;;   (let ((current (calculate-column-extents rows column)))
;;     (if (equal current goal)
;;         (return-from solve-column rows))
;;     (previous-white-index rows 0 column )
        

(defun solve-puzzle (&key file-name (columns *columns*) (rows *rows*))
  (let (;; (board (make-array (list (length columns) (length rows)) :element-type t :initial-element nil))
        (new-rows (calculate-full-row-states rows)))
    (inner-solve new-rows 0 0 columns)
    (to-png file-name new-rows)
    new-rows))
    ;; (fill-board board new-rows)
    ;; (to-png file-name board)))
