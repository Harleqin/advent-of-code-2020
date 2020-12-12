(in-package #:cl-user)

(defpackage #:aoc-2020-11
  (:use #:cl
        #:alexandria
        #:aoc-2020
        #:arrows
        #:cl-ppcre
        #:let-plus))

(in-package #:aoc-2020-11)

(defun aoc-11a (&optional
                  (input (read-matrix #p"11"))
                  show-area-p)
  (rule-fixpoint input #'rule-a show-area-p))

(defun rule-fixpoint (input rule show-area-p)
  (loop :for prev := nil :then area
        :for area := input :then (apply-rule rule area)
        :for round :upfrom 0
        :when (eql show-area-p :all)
          :do (print-matrix area)
        :when (equalp prev area)
          :return (values (count #\# (array-flat-view area))
                          round
                          (when show-area-p (print-matrix area)))))

(defun rule-a (area y x)
  (ccase (aref area y x)
    (#\. #\.)
    (#\L (if (zerop (count-occ area y x))
             #\#
             #\L))
    (#\# (if (>= (count-occ area y x) 4)
             #\L
             #\#))))

(defun apply-rule (rule area)
  (let ((new (make-array (array-dimensions area))))
    (loop :for y :below (array-dimension area 0)
          :do (loop :for x :below (array-dimension area 1)
                    :do (setf (aref new y x)
                              (funcall rule area y x))))
    new))

(defun count-occ (area y x)
  (loop :for (yi xi) :in (list (list (1- y) (1- x))
                               (list (1- y) x)
                               (list (1- y) (1+ x))
                               (list y (1- x))
                               (list y (1+ x))
                               (list (1+ y) (1- x))
                               (list (1+ y) x)
                               (list (1+ y) (1+ x)))
        :when (array-in-bounds-p area yi xi)
          :count (char= (aref area yi xi) #\#)))

(defun aoc-11b (&optional
                  (input (read-matrix #p"11"))
                  show-area-p)
  (rule-fixpoint input #'rule-b show-area-p))

(defun rule-b (area y x)
  (ccase (aref area y x)
    (#\. #\.)
    (#\L (if (zerop (count-los area y x))
             #\#
             #\L))
    (#\# (if (>= (count-los area y x) 5)
             #\L
             #\#))))

(defun count-los (area y x)
  (loop :for (dy dx) :in '((-1 -1)
                           (-1 0)
                           (-1 1)
                           (0 -1)
                           (0 1)
                           (1 -1)
                           (1 0)
                           (1 1))
        :for seat := (find-seat area y x dy dx)
        :count (eql seat #\#)))

(defun find-seat (area y x dy dx)
  (loop :for yi := (+ y dy) :then (+ yi dy)
        :for xi := (+ x dx) :then (+ xi dx)
        :for e := (when (array-in-bounds-p area yi xi)
                    (aref area yi xi))
        :while e
        :when (member e '(#\# #\L))
          :return e))
