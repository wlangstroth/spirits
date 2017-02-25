;;;; spirits.lisp

;; The thesis is that entries and events are different. e.g. the entry
;; "We are going to the park next Tuesday" should populate an event with the
;; date for next Tuesday.
;; Future events and past events should also be different, since past events
;; are certainties and future events are not.

(in-package #:spirits)

;; entries can be stored as a hash table, using entry-timestamp as the key
(defvar *entries* (make-hash-table)
  "Entries are timestamped text notes")

(defvar *hashtags* (make-hash-table)
  "Index of hashtags and their associated entries")

(defvar *events* nil
  "Events are time-dependent instances parsed from entries")

(defclass entry ()
  ((timestamp :accessor entry-timestamp
              :initarg :timestamp
              :initform (get-universal-time))
   (text :accessor entry-text
         :initarg :text
         :initform "")))

(defun plist-from-entry (entry)
  (list :timestamp (entry-timestamp entry)
        :text (entry-text entry)))

(defun save-entries ()
  (let ((entries nil))
    (maphash #'(lambda (key value)
                 (push (list :timestamp key :text value) entries))
             *entries*)
    (with-open-file (out "entries.db"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print entries out))
    (length entries))))

(defun entry-from-plist (entry-plist)
  (make-instance 'entry
                 :timestamp (getf entry-plist :timestamp)
                 :text (getf entry-plist :text)))

(defun entry-hash-from-list (entry-list)
  (let ((entries (make-hash-table)))
    (mapcar
     #'(lambda (plist)
         (let ((e (entry-from-plist plist)))
           (setf (gethash (entry-timestamp e) entries) (entry-text e))))
     entry-list)
    entries))

(defun load-entries ()
  (let ((entries nil))
    (with-open-file (in "entries.db")
      (with-standard-io-syntax
        (setf entries (read in))))
    (setf *entries* (entry-hash-from-list entries))))

(defun add-entry (text &optional (timestamp (get-universal-time)))
  (setf (gethash timestamp *entries*) text))

(defun list-entries ()
  (maphash (lambda (key value)
             (format t "~a | ~a~%" key value))
           *entries*))

(defun past (&optional entry)
  (cond ((null entry)
         (list-entries))
        (t
         (add-entry (getf entry :timestamp)
                    (getf entry :text)))))

(defun present (&optional text)
  "Either see the state of the present, or create an entry to be placed in
   the immediate past"
  (cond ((null text) "Not implemented")
        (t (add-entry text)
           (save-entries))))

(defun future (&optional event)
  "Either upcoming events or create a future event"
  (cond ((null event) (future-events))
        (t "Not implemented")))


;;; Entry processing

(defun hashtag-p (token)
  (equal (subseq token 0 1) "#"))

(defun dehash (token)
  (intern (subseq token 1)))

(defun hashtags-from-entry (entry)
  (hashtags-from-text (entry-text entry)))

(defun hashtags-from-text (text)
  (let ((tokens (split-sequence #\Space text)))
    (mapcar #'dehash
            (remove-if-not #'hashtag-p tokens))))
