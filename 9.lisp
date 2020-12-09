(in-package #:cl-user)

(defpackage #:aoc-2020-9
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-9)

(defun aoc-9a (&optional
                 (input (read-integers #p"9" 'vector))
                 (window-size 25))
  (loop :for i :from window-size :below (length input)
        :for sums := (sums input i (+ i window-size))
        :for n :=(aref input (+ i window-size))
        :unless (member n sums)
          :return n))

(defun sums (v start end)
  (-> (loop :for i :from start :below end
            :nconc (loop :for j :from i :below end
                         :collect (+ (aref v i) (aref v j))))
      remove-duplicates))

(defun aoc-9b (&optional
                 (input (read-integers #p"9" 'vector))
                 (window-size 25))
  (let ((x (aoc-9a input window-size))
        (i 0)
        (j 0)
        (sum 0))
    (loop
      (cond ((< sum x)
             (incf sum (aref input j))
             (incf j))
            ((> sum x)
             (decf sum (aref input i))
             (incf i))
            ((= sum x)
             (return (+ (reduce #'min (subseq input i j))
                        (reduce #'max (subseq input i j)))))))))
