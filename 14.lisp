(in-package #:cl-user)

(defpackage #:aoc-2020-14
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-14)

(defun aoc-14a (&optional (input (lines #p"14")))
  (loop :with mask := 0
        :with mem := (make-hash-table)
        :for line :in input
        :do (cond ((register-groups-bind ((#'read-mask new-mask))
                       ("mask = (.+)" line)
                     (setf mask new-mask)))
                  ((register-groups-bind ((#'parse-integer address value))
                       ("mem\\[(\\d+)\\] = (\\d+)" line)
                     (setf (gethash address mem)
                           (apply-mask mask value)))))
        :finally (return (reduce #'+ (hash-table-values mem)))))

(defun read-mask (string)
  (loop :for c :across (reverse string)
        :for i :upfrom 0
        :for d := (digit-char-p c 2)
        :when d
        :collect (cons i d)))

(defun apply-mask (mask integer)
  (loop :for (i . d) :in mask
        :for int := (dpb d (byte 1 i) integer)
          :then (dpb d (byte 1 i) int)
        :finally (return int)))

(defun aoc-14b (&optional (input (lines #p"14")))
  (loop :with mask := nil
        :with mem := (make-hash-table)
        :for line :in input
        :do (cond ((register-groups-bind ((#'reverse new-mask))
                       ("mask = (.+)" line)
                     (setf mask new-mask)))
                  ((register-groups-bind ((#'parse-integer address value))
                       ("mem\\[(\\d+)\\] = (\\d+)" line)
                     (apply-mask-b mem mask address value))))
        :finally (return (reduce #'+ (hash-table-values mem)))))

(defun apply-mask-b (mem mask address value)
  (let ((addresses
          (loop :with as := (list address)
                :for m :across mask
                :for i :upfrom 0
                :do (case m
                      (#\1 (setf as
                                 (mapcar (lambda (a)
                                           (logior (ash 1 i) a))
                                         as)))
                      (#\X (setf as
                                 (remove-duplicates
                                  (mapcan (lambda (a)
                                            (list (dpb 0 (byte 1 i) a)
                                                  (dpb 1 (byte 1 i) a)))
                                          as)))))
                :finally (return as))))
    (dolist (address addresses)
      (setf (gethash address mem) value))))
