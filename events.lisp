(in-package #:spirits)

;; Something like "dinner tomorrow" should be an event, too. It would be a good
;; way to prepare. We need a planner.

;; The predicate - should it be in present tense to accommodate all tenses? Or
;; change once it passess from future to past?

;; How often do the events have to be processed to pass events into the past or
;; perform synonym reductions?

(defvar *events* nil
  "Events are time-dependent instances parsed from entries")

(defclass event ()
  ((timestamp
    :initarg :timestamp
    :initform (chronograph:iso-now)
    :accessor event-timestamp)
   (subject
    :initarg :subject
    :initform "Will"
    :accessor event-subject)
   (predicate
    :initarg :predicate
    :initform ""
    :accessor event-predicate)
   (object
    :initarg :object
    :initform ""
    :accessor event-object)
   (context
    :initarg :context
    :initform ()
    :accessor event-context)))

(defun insert-event (timestamp subject predicate object context)
  (push
   (make-instance 'event
                  :timestamp timestamp
                  :subject subject
                  :predicate predicate
                  :object object
                  :context context)
   *events*))

(defun add-event (subject predicate &optional (object "") (context ()))
  (let ((new-event
         (make-instance 'event
                  :subject subject
                  :predicate predicate
                  :object object
                  :context context)))
    (push new-event *events*)
    (plist-from-event new-event)))

(defun future-events ()
  nil)

(defun plist-from-event (event)
  (list :timestamp (event-timestamp event)
        :subject (event-subject event)
        :predicate (event-predicate event)
        :object (event-object event)))

(defun event-from-plist (event-plist)
  (make-instance 'event
                 :timestamp (getf event-plist :timestamp)
                 :subject (getf event-plist :subject)
                 :predicate (getf event-plist :predicate)
                 :object (getf event-plist :object)
                 :context (getf event-plist :context)))

(defun load-events ()
  (let ((plists nil))
    (with-open-file (in "events.db")
      (with-standard-io-syntax
        (setf plists (read in))))
    (setf *events* (mapcar #'event-from-plist plists))
    (format t "~%Loaded ~d events" (length *events*))))

(defun save-events ()
  (with-open-file (out "events.db"
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print (mapcar #'plist-from-event *events*) out)))
  (format t "Saved ~d events" (length *events*)))

(load-events)
