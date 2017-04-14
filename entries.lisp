(in-package #:spirits)

(defclass entry ()
  ((timestamp
    :accessor entry-timestamp
    :initarg :timestamp
    :initform (get-universal-time))
   (text
    :accessor entry-text
    :initarg :text
    :initform "")))

(defvar *entries* nil
  "Entries are timestamped text notes")

(defun plist-from-entry (entry)
  (list :timestamp (entry-timestamp entry)
        :text (entry-text entry)))

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
    (format t "~%Loaded ~d entries" (length *entries*))))

(defun save-entries ()
  (with-open-file (out "entries.db"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'plist-from-entry *entries*) out)))
  (format t "Saved ~d entries" (length *entries*)))

(defun entries-with-text (query)
  (loop for e in *entries*
     if (search query (entry-text e))
     collect e))

;;; Entry processing

(defun hashtag-p (token)
  (equal (subseq token 0 1) "#"))

(defun dehash (token)
  (intern (subseq token 1)))

(defun hashtags-from-entry (entry)
  (hashtags-from-text (entry-text entry)))

(defun entry-tokens (entry)
  (split-sequence #\Space (entry-text entry)))

(defun hashtags-from-text (text)
  (let ((tokens (split-sequence #\Space text)))
    (mapcar #'dehash
            (remove-if-not #'hashtag-p tokens))))

(defun sort-entries ()
  (sort *entries*
        #'(lambda (e f)
            (> (entry-timestamp e) (entry-timestamp f)))))

(defparameter *stopwords*
  '("really" "very" "a"))

(defun in-stopwords-p (token)
  (member token *stopwords* :test #'string=))

(defparameter *tag-meanings*
  '(:need "Ran out of an item"
    :bought "Purchased an item or all the items in the list"
    :ate "Used up items in a standard recipe"))

(defun trim-spaces (text)
  "Trims the spaces leppft from splitting up clauses at periods"
  (string-left-trim '(#\space) text))

(defun split-clauses (text)
  "Breaks up entries by period"
  (mapcar #'trim-spaces
          (split-sequence #\. text :remove-empty-subseqs t)))

(defun parse-need-clause (clause) clause)

(defun tokenize-clause (clause) clause)

(defun scan-tokens (tokens*) tokens*)

(defun parse-entry-tokens (tokens*) tokens*)

(defparameter *synonyms* nil)

(load-entries)

(defun convert-timestamp (entry)
  (let ((stamp (entry-timestamp entry)))
    (setf stamp (chronograph:iso-from-universal stamp))))
