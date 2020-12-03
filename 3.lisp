(in-package #:cl-user)

(defpackage #:aoc-2020-3
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows))

(in-package #:aoc-2020-3)

(defun aoc-3a (&optional (input #p"3"))
  (let ((map (read-matrix input)))
    (check-slope map 3 1)))

(defun check-slope (map dx dy)
  (loop :with width := (array-dimension map 1)
        :for x := dx :then (mod (+ x dx) width)
        :for y :from dy :by dy
        :while (< y (array-dimension map 0))
        :count (char= (aref map y (mod x width)) #\#)))

(defun aoc-3b (&optional (input #p"3"))
  (let ((map (read-matrix input)))
    (reduce #'*
            (loop :for (x y) :in '((1 1)
                                   (3 1)
                                   (5 1)
                                   (7 1)
                                   (1 2))
                  :collect (check-slope map x y)))))
