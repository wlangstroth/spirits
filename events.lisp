(in-package #:spirits)

(defvar *events* nil
  "Events are time-dependent instances parsed from entries")

(defclass event ()
  ((timestamp
    :initarg :timestamp
    :initform "")))

(defun future-events ()
  nil)
