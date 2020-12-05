(in-package #:cl-user)

(defpackage #:aoc-2020-5
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2020-5)

(defstruct boarding-pass
  raw
  row
  column
  seat-id)

(defun aoc-5a (&optional (input (read-file-into-string "5")))
  (with-input-from-string (in input)
    (loop :for pass :in (read-boarding-passes in)
          :maximize (boarding-pass-seat-id pass))))

(defun read-boarding-passes (stream)
  (loop :for line := (read-line stream nil)
        :while line
        :collect (read-boarding-pass line)))

(defun read-boarding-pass (string)
  (let ((row (read-binary (subseq string 0 7)
                          #\F #\B))
        (column (read-binary (subseq string 7)
                             #\L #\R)))
    (make-boarding-pass :raw string
                        :row row
                        :column column
                        :seat-id (+ (* row 8)
                                    column))))

(defun read-binary (string zero one)
  (-<>> string
        (map 'string (lambda (c)
                       (cond ((char= c zero) #\0)
                             ((char= c one) #\1)
                             (t (error "unknown char ~s" c)))))
        (parse-integer <> :radix 2)))

(defun aoc-5b (&optional (input (read-file-into-string "5")))
  (with-input-from-string (in input)
    (let* ((passes (read-boarding-passes in))
           (seat-ids (sort (mapcar #'boarding-pass-seat-id passes)
                           #'<)))
      (loop :for (a b) :on seat-ids
            :thereis (when (= (- b a) 2)
                       (1+ a))))))
