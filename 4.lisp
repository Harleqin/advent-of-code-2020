(in-package #:cl-user)

(defpackage #:aoc-2020-4
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre))

(in-package #:aoc-2020-4)

(defun aoc-4a (&optional (input (read-file-into-string "4")))
  (with-input-from-string (in input)
    (loop :for passport := (read-passport in)
          :while passport
          :count (valid-passport-p passport))))

(defun read-passport (stream)
  (loop :for line := (read-line stream nil)
        :while (plusp (length line))
        :nconc (passport-fields line)))

(defun passport-fields (string)
  (->> string
       (split "\\s+")
       (mapcar (lambda (s) (split ":" s)))))

(defun valid-passport-p (passport)
  (subsetp '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")
           (mapcar #'car passport)
           :test #'string=))

(defun aoc-4b (&optional (input (read-file-into-string "4")))
  (with-input-from-string (in input)
    (loop :for passport := (read-passport in)
          :while passport
          :count (really-valid-passport-p passport))))

(defun really-valid-passport-p (passport)
  (and (valid-passport-p passport)
       (loop :for (k v) :in passport
             :always (valid-field-p k v))))

(defun valid-field-p (k v)
  (switch (k :test #'string=)
    ("byr" (<= 1920 (parse-integer v) 2002))
    ("iyr" (<= 2010 (parse-integer v) 2020))
    ("eyr" (<= 2020 (parse-integer v) 2030))
    ("hgt" (register-groups-bind ((#'parse-integer n)
                                  u)
               ("^(\\d+)(cm|in)$" v)
             (switch (u :test #'string=)
               ("cm" (<= 150 n 193))
               ("in" (<= 59 n 76)))))
    ("hcl" (scan "^#[0-9a-f]{6}$" v))
    ("ecl" (scan "^(amb|blu|brn|gry|grn|hzl|oth)$" v))
    ("pid" (scan "^\\d{9}$" v))
    ("cid" t)))
