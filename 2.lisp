(in-package #:cl-user)

(defpackage #:aoc-2020-2
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:cl-ppcre))

(in-package #:aoc-2020-2)

(defun aoc-2a (&optional (input (read-file-into-string "2")))
  (with-input-from-string (in input)
    (loop :for line := (read-line in nil)
          :while line
          :count (correct-password-a-p line))))

(defun correct-password-a-p (line)
  (register-groups-bind ((#'parse-integer min max)
                         ((rcurry #'char 0) letter)
                         password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" line)
    (<= min (count letter password) max)))

(defun aoc-2b (&optional (input (read-file-into-string "2")))
  (with-input-from-string (in input)
    (loop :for line := (read-line in nil)
          :while line
          :count (correct-password-b-p line))))

(defun correct-password-b-p (line)
  (register-groups-bind ((#'parse-integer a b)
                         ((rcurry #'char 0) letter)
                         password)
      ("(\\d+)-(\\d+) (\\w): (\\w+)" line)
    (xor (char= letter (char password (1- a)))
         (char= letter (char password (1- b))))))
