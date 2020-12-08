(in-package #:cl-user)

(defpackage #:aoc-2020-8
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-8)

(defun aoc-8a (&optional (input (lines #p"8")))
  (let ((mem (parse-program input)))
    (first (run mem))))

(defun parse-program (lines)
  (map 'vector
       (lambda (line)
         (list (intern (string-upcase (subseq line 0 3)) :keyword)
               (parse-integer line :start 4)))
       lines))

(defun run (mem)
  (loop :for ip := 0 :then next-ip
        :for (acc next-ip loopp) := (op! mem ip 0) :then (op! mem ip acc)
          :thereis (cond ((= next-ip (length mem))
                          (list acc ip t))
                         ((not (array-in-bounds-p mem next-ip))
                          (list acc ip nil))
                         (loopp
                          (list acc ip nil)))))

(defun op! (mem ip acc)
  (let+ (((op &optional n) (aref mem ip)))
    (setf (aref mem ip) (list :seen))
    (ecase op
      (:acc (list (+ acc n) (1+ ip)))
      (:jmp (list acc (+ ip n)))
      (:nop (list acc (1+ ip)))
      (:seen (list acc ip t)))))

(defun aoc-8b (&optional (input (lines #p"8")))
  (let ((mem (parse-program input)))
    (loop :for i :from 0 :below (length mem)
          :for mem* := (toggle-instruction mem i)
          :for (acc ip okp) := (run mem*)
          :thereis (when okp
                     acc))))

(defun toggle-instruction (mem i)
  (let+ ((mem* (map 'vector #'copy-seq mem))
         (inst (aref mem* i)))
    (case (car inst)
      (:jmp (setf (car inst) :nop))
      (:nop (setf (car inst) :jmp)))
    mem*))
