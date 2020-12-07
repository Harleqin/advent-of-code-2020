(in-package #:cl-user)

(defpackage #:aoc-2020-7
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-7)

(defun aoc-7a (&optional (input (lines #p"7")))
  (let ((rules (mapcar #'parse-rule input))
        (contained-by (make-hash-table :test #'equal)))
    (loop :for (parent . children) :in rules
          :do (loop :for (n . colour) :in children
                    :do (pushnew parent (gethash colour contained-by))))
    (loop :for bags := '("shiny gold") :then new
          :for contained-in := (-> (mapcan (lambda (c)
                                             (copy-list (gethash c contained-by)))
                                           bags)
                                   (remove-duplicates :test #'equal))
          :for new := contained-in
            :then (remove-if (lambda (c)
                               (member c others :test #'equal))
                             contained-in)
          :for others := new :then (append new others)
          :while new
          :finally (return (length others)))))

(defun parse-rule (string)
  (let+ (((parent-colour rest) (split " bags contain " string))
         (children (-<>> rest
                         (regex-replace " bags?\\." <> "")
                         (split " bags?, ")
                         (mapcar (lambda (s)
                                   (register-groups-bind ((#'parse-integer n)
                                                          colour)
                                       ("(\\d+) (.+)$" s)
                                     (cons n colour))))
                         (remove nil))))
    (cons parent-colour children)))

(defun aoc-7b (&optional (input (lines #p"7")))
  (let ((rules (mapcar #'parse-rule input))
        (contains (make-hash-table :test #'equal)))
    (loop :for (parent . children) :in rules
          :do (loop :for child :in children
                    :do (push child (gethash parent contains))))
    (loop :for bags := '((1 . "shiny gold")) :then new
          :for new := (loop :for (n . colour) :in bags
                            :for children := (gethash colour contains)
                            :append (mapcar (lambda (child)
                                              (cons (* n (car child))
                                                    (cdr child)))
                                            children))
          :while new
          :sum (reduce #'+ (mapcar #'car new)))))
