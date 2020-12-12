(in-package #:cl-user)

(defpackage #:aoc-2020-12
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-12)

(defun aoc-12a (&optional (input (lines #p"12")))
  (loop :for line :in input
        :for position := '(0 0) :then next-pos
        :for facing := '(0 1) :then next-facing
        :for (next-pos next-facing) := (make-it-so line position facing)
        :finally (return (reduce #'+ next-pos))))

(defun make-it-so (string position facing)
  (or (register-groups-bind ((#'intern-keyword instruction)
                             (#'parse-integer value))
          ("([NESWLRF])(\\d+)" string)
        (ccase instruction
          (:n (list (move position value '(-1 0))
                    
                    facing))
          (:e (list (move position value '(0 1))
                    facing))
          (:s (list (move position value '(1 0))
                    facing))
          (:w (list (move position value '(0 -1))
                    facing))
          (:l (list position
                    (turn facing :l value)))
          (:r (list position
                    (turn facing :r value)))
          (:f (list (move position value facing)
                    facing))))
      (signal "Could not parse ~s" string)))

(defun move (position value dir)
  (loop :for v :in position
        :and dv :in dir
        :collect (+ v (* value dv))))

(defun turn (facing way value)
  (let+ ((angle (cond-> (/ value 90)
                        ((eql way :r) -)
                        (t (mod 4))))
         ((y x) facing))
    (ccase angle
      (0 facing)
      (1 (list (- x) y))
      (2 (list (- y) (- x)))
      (3 (list x (- y))))))

(defun aoc-12b (&optional (input (lines #p "12")))
  (loop :for line :in input
        :for position := '(0 0) :then next-pos
        :for waypoint := '(-1 10) :then next-waypoint
        :for (next-pos next-waypoint) := (move-by-waypoint line
                                                           position
                                                           waypoint)
        :finally (return (reduce #'+ next-pos))))

(defun move-by-waypoint (string position waypoint)
  (or (register-groups-bind ((#'intern-keyword instruction)
                             (#'parse-integer value))
          ("([NESWLRF])(\\d+)" string)
        (ccase instruction
          (:n (list position
                    (move waypoint value '(-1 0))))
          (:e (list position
                    (move waypoint value '(0 1))))
          (:s (list position
                    (move waypoint value '(1 0))))
          (:w (list position
                    (move waypoint value '(0 -1))))
          (:l (list position
                    (turn waypoint :l value)))
          (:r (list position
                    (turn waypoint :r value)))
          (:f (list (move position value waypoint)
                    waypoint))))
      (signal "Could not parse ~s" string)))

;; It's funny that I could reuse the move and turn functions exactly as written
;; in the first part.
