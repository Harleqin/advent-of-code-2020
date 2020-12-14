(in-package #:cl-user)

(defpackage #:aoc-2020-13
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-13)

(defun aoc-13a (&optional (input (lines #p"13")))
  (let ((now (parse-integer (first input)))
        (buses (-<>> (second input)
                     (split #\,)
                     (remove "x" <> :test #'equal)
                     (mapcar #'parse-integer)))
        min-dt
        min-bus)
    (loop :for bus :in buses
          :for dt := (- (nth-value 1 (ceiling now bus)))
          :when (or (null min-dt) (< dt min-dt))
            :do (setf min-dt dt
                      min-bus bus))
    (* min-dt min-bus)))

(defun aoc-13b (&optional
                  (input (second (lines #p"13"))))
  (first (reduce #'combine (parse-buses-b input))))

(defun parse-buses-b (input)
  (loop :for bus :in (split #\, input)
        :for i :from 0
        :when (string/= bus "x")
          :collect (list i (parse-integer bus))))

(defun combine (a b)
  (let+ (((offset0 period0) a)
         ((offset1 period1) b))
    (list (loop :for time :from offset0 :by period0
                :when (zerop (mod (+ time offset1) period1))
                  :return time)
          (* period0 period1))))
