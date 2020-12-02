(in-package #:cl-user)

(defpackage #:aoc-2020
  (:use #:cl)
  (:export #:read-integers))

(in-package #:aoc-2020)

(defun read-integers (filename &optional (type 'list))
  (coerce (with-open-file (in filename)
            (loop :for line := (read-line in nil)
                  :while line
                  :append (loop :for s := (substitute #\space #\, line)
                                :for (i pos)
                                  := (multiple-value-list
                                      (parse-integer s
                                                     :start (or pos 0)
                                                     :junk-allowed t))
                                :while i
                                :collect i)))
          type))
