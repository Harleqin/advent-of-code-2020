(in-package #:cl-user)

(defpackage #:aoc-2020-6
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2020-6)

(defun aoc-6a (&optional (input (read-file-into-string "6")))
  (with-input-from-string (in input)
    (loop :for group :in (read-groups in #'logior)
          :sum (logcount group))))

(defun read-groups (stream comb)
  (loop :for group := (read-group stream comb)
        :while group
        :collect group))

(defun read-group (stream comb)
  (some->> (loop :for line := (read-line stream nil)
                 :while (plusp (length line))
                 :collect (group-bitmap line))
           (reduce comb)))

(defun group-bitmap (string)
  (reduce #'logior
          (map 'vector
               (lambda (c)
                 (ash 1 (- (char-code c) (char-code #\a))))
               string)))

(defun aoc-6b (&optional (input (read-file-into-string "6")))
  (with-input-from-string (in input)
    (loop :for group :in (read-groups in #'logand)
          :sum (logcount group))))
