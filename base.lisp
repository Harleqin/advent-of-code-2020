(in-package #:cl-user)

(defpackage #:aoc-2020
  (:use #:cl #:arrows)
  (:export #:array-flat-view
           #:lines
           #:print-matrix
           #:read-blocks
           #:read-integers
           #:read-matrix))

(in-package #:aoc-2020)

(defgeneric read-integers (source &optional type))

(defmethod read-integers ((filename pathname) &optional (type 'list))
  (with-open-file (in filename)
    (read-integers in type)))

(defmethod read-integers ((string string) &optional (type 'list))
  (with-input-from-string (in string)
    (read-integers in type)))

(defmethod read-integers ((in stream) &optional (type 'list))
  (coerce (loop :for line := (read-line in nil)
                :while line
                :append (loop :for s := (substitute #\space #\, line)
                              :for (i pos)
                                := (multiple-value-list
                                    (parse-integer s
                                                   :start (or pos 0)
                                                   :junk-allowed t))
                              :while i
                              :collect i))
          type))

(defgeneric lines (source))

(defmethod lines ((stream stream))
  (loop :for line := (read-line stream nil)
        :while line
        :collect line))

(defmethod lines ((pathname pathname))
  (with-open-file (in pathname)
    (lines in)))

(defmethod lines ((string string))
  (with-input-from-string (in string)
    (lines in)))

(defun read-matrix (source)
  "Read a rectangular 2d matrix from source, which can be a pathname, a string,
or a stream."
  (let* ((lines (lines source))
         (width (length (first lines)))
         (height (length lines))
         (array (make-array (list height width))))
    (loop :for y :below height
          :for line :in lines
          :do (loop :for x :below width
                    :for char :across line
                    :do (setf (aref array y x) char)))
    array))

(defun print-matrix (matrix &optional lookup-alist)
  (flet ((lookup (e)
           (if lookup-alist
               (cdr (assoc e lookup-alist))
               e)))
    (dotimes (y (array-dimension matrix 0))
      (dotimes (x (array-dimension matrix 1))
        (princ (lookup (aref matrix y x))))
      (terpri))))

(defun read-blocks (source)
  "Read a string that consists of lines that are split into groups by empty
lines into a list of lists of strings."
  (->> (lines source)
       (split-sequence:split-sequence "")))

(defun array-flat-view (array)
  (make-array (array-total-size array)
              :element-type (array-element-type array)
              :displaced-to array))
