(in-package #:cl-user)

(defpackage #:aoc-2020
  (:use #:cl)
  (:export #:read-integers
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

(defgeneric read-matrix (source))

(defmethod read-matrix ((pathname pathname))
  (with-open-file (in pathname)
    (read-matrix in)))

(defmethod read-matrix ((string string))
  (with-input-from-string (in string)
    (read-matrix in)))

(defmethod read-matrix ((in stream))
  (read-matrix (loop :for line := (read-line in nil)
                     :while line
                     :collect line)))

(defmethod read-matrix ((lines cons))
  (let* ((width (length (first lines)))
         (height (length lines))
         (array (make-array (list height width))))
    (loop :for y :below height
          :for line :in lines
          :do (loop :for x :below width
                    :for char :across line
                    :do (setf (aref array y x) char)))
    array))
