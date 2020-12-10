(in-package #:cl-user)

(defpackage #:aoc-2020-10
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-10)

(defun aoc-10a (&optional (input (read-integers #p"10")))
  (let ((adapters (sort input #'<))
        (counts (make-array 4 :initial-element 0)))
    (loop :for (a b) :on (cons 0 adapters)
          :while b
          :do (incf (aref counts (- b a))))
    (* (aref counts 1)
       (1+ (aref counts 3)))))

(defun aoc-10b (&optional (input (read-integers #p"10")))
  (let* ((adapters (sort (coerce (cons 0 input) 'vector) #'<))
         (counts (make-array (length adapters)
                             :initial-element 0)))
    (setf (aref counts 0) 1)
    (loop :for i :from 1 :below (length adapters)
          :do (setf (aref counts i)
                    (loop :for j :downfrom (1- i)
                          :repeat 3
                          :while (and (not (minusp j))
                                      (>= (aref adapters j)
                                          (- (aref adapters i) 3)))
                          :sum (aref counts j))))
    (aref counts (1- (length counts)))))
