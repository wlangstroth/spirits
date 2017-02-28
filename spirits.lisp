;;;; spirits.lisp

;; The thesis is that entries and events are different. e.g. the entry
;; "We are going to the park next Tuesday" should populate an event with the
;; date for next Tuesday.
;; Future events and past events should also be different, since past events
;; are certainties and future events are not.

(in-package #:spirits)

;; entries can be stored as a hash table, using entry-timestamp as the key
(defvar *entries* nil
  "Entries are timestamped text notes")

(defvar *events* nil
  "Events are time-dependent instances parsed from entries")

(defclass entry ()
  ((timestamp
    :accessor entry-timestamp
    :initarg :timestamp
    :initform (get-universal-time))
   (text
    :accessor entry-text
    :initarg :text
    :initform "")))

(defun plist-from-entry (entry)
  (list :timestamp (entry-timestamp entry)
        :text (entry-text entry)))

(defun save-entries ()
  (with-open-file (out "entries.db"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'plist-from-entry *entries*) out)))
  (length *entries*))

(defun entry-from-plist (entry-plist)
  (make-instance 'entry
                 :timestamp (getf entry-plist :timestamp)
                 :text (getf entry-plist :text)))

(defun entry-line (entry)
  (format t "~a | ~a~%"
          (entry-timestamp entry)
          (entry-text entry)))

(defun load-entries ()
  (let ((plists nil))
    (with-open-file (in "entries.db")
      (with-standard-io-syntax
        (setf plists (read in))))
    (setf *entries* (mapcar #'entry-from-plist plists))
    (length *entries*)))

(defun past (&optional entry)
  (cond ((null entry)
         (mapcar #'entry-line *entries*)
         (length *entries*))
        (t (append *entries* `(,entry)))))

(defun last-n (&optional (lines 1))
  (mapcar #'entry-line (subseq *entries* 0 lines))
  lines)

(defun present (&optional text)
  "Either see the state of the present (todo/shopping lists, trades in play),
or create an entry to be placed in the immediate past"
  (cond ((null text) (trading:trade-status))
        (t
         (push
          (make-instance 'entry
                         :text text
                         :timestamp (get-universal-time))
          *entries*)
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

(defun sort-entries ()
  (sort *entries*
        #'(lambda (e f)
            (> (entry-timestamp e) (entry-timestamp f)))))

(defun entries-with-text (query)
  (loop for e in *entries*
     if (search query (entry-text e))
     collect e))
