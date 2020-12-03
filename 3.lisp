(in-package #:cl-user)

(defpackage #:aoc-2020-3
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows))

(in-package #:aoc-2020-3)

(defun aoc-3a (&optional (input (read-file-into-string "3")))
  (let* ((lines (with-input-from-string (in input)
                  (loop :for line := (read-line in nil)
                        :while line
                        :collect line)))
         (map (read-matrix lines)))
    (check-slope map 3 1)))

(defun check-slope (map dx dy)
  (loop :with width := (array-dimension map 1)
        :for x := dx :then (mod (+ x dx) width)
        :for y :from dy :by dy
        :while (< y (array-dimension map 0))
        :count (char= (aref map y (mod x width)) #\#)))

(defun aoc-3b (&optional (input (read-file-into-string "3")))
  (let* ((lines (with-input-from-string (in input)
                  (loop :for line := (read-line in nil)
                        :while line
                        :collect line)))
         (map (read-matrix lines)))
    (reduce #'*
            (loop :for (x y) :in '((1 1)
                                   (3 1)
                                   (5 1)
                                   (7 1)
                                   (1 2))
                  :collect (check-slope map x y)))))
