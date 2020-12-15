(in-package #:aoc-2020)

(defun test (description bool)
  (format t "~a: ~s~%" description bool)
  bool)

(defmacro tests (&body tests)
  `(every #'identity
          (list ,@tests)))

(defun test-aoc ()
  (tests
    (test "1a" (= (aoc-2020-1::aoc-1a) 996075))
    (test "1b" (= (aoc-2020-1::aoc-1b) 51810360))
    (test "2a" (= (aoc-2020-2::aoc-2a) 460))
    (test "2b" (= (aoc-2020-2::aoc-2b) 251))
    (test "3a" (= (aoc-2020-3::aoc-3a) 156))
    (test "3b" (= (aoc-2020-3::aoc-3b) 3521829480))
    (test "4a" (= (aoc-2020-4::aoc-4a) 210))
    (test "4b" (= (aoc-2020-4::aoc-4b) 131))
    (test "5a" (= (aoc-2020-5::aoc-5a) 996))
    (test "5b" (= (aoc-2020-5::aoc-5b) 671))
    (test "6a" (= (aoc-2020-6::aoc-6a) 6437))
    (test "6b" (= (aoc-2020-6::aoc-6b) 3229))
    (test "7a" (= (aoc-2020-7::aoc-7a) 238))
    (test "7b" (= (aoc-2020-7::aoc-7b) 82930))
    (test "8a" (= (aoc-2020-8::aoc-8a) 2025))
    (test "8b" (= (aoc-2020-8::aoc-8b) 2001))
    (test "9a" (= (aoc-2020-9::aoc-9a) 756008079))
    (test "9b" (= (aoc-2020-9::aoc-9b) 93727241))
    (test "10a" (= (aoc-2020-10::aoc-10a) 2176))
    (test "10b" (= (aoc-2020-10::aoc-10b) 18512297918464))
    (test "11a" (= (aoc-2020-11::aoc-11a) 2329))
    (test "11b" (= (aoc-2020-11::aoc-11b) 2138))
    (test "12a" (= (aoc-2020-12::aoc-12a) 998))
    (test "12b" (= (aoc-2020-12::aoc-12b) 71586))
    (test "13a" (= (aoc-2020-13::aoc-13a) 4135))
    (test "13b" (= (aoc-2020-13::aoc-13b) 640856202464541))
    (test "14a" (= (aoc-2020-14::aoc-14a) 9967721333886))
    (test "14b" (= (aoc-2020-14::aoc-14b) 4355897790573))))
