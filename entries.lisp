(in-package #:spirits)

(defvar *entries* nil
  "Entries are timestamped text notes")

(defclass entry ()
  ((timestamp
    :accessor entry-timestamp
    :initarg :timestamp
    :initform (get-universal-time))
   (text
    :accessor entry-text
    :initarg :text
    :initform "")))

(defun save-entries ()
  (with-open-file (out "entries.db"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'plist-from-entry *entries*) out)))
  (length *entries*))
