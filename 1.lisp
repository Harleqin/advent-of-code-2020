(in-package #:cl-user)

(defpackage #:aoc-2020-1
  (:use #:cl
        #:aoc-2020))

(in-package #:aoc-2020-1)

(defun aoc-1a (&optional (ints (read-integers #p"1")))
  (loop :for (a . rest) :on ints
          :thereis (loop :for b :in rest
                           :thereis (when (= (+ a b) 2020)
                                      (* a b)))))

(defun aoc-1b (&optional (ints (read-integers #p"1")))
  (loop :for (a . rest) :on ints
          :thereis (loop :for (b . rest) :on rest
                           :thereis (loop :for c :in rest
                                            :thereis (when (= (+ a b c) 2020)
                                                       (* a b c))))))
