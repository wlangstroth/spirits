(in-package #:spirits)

;; Something like "dinner tomorrow" should be an event, too. It would be a good
;; way to prepare. We need a planner.

;; The predicate - should it be in present tense to accommodate all tenses? Or
;; change once it passess from future to past?

(defvar *events* nil
  "Events are time-dependent instances parsed from entries")

(defclass event ()
  ((timestamp
    :initarg :timestamp
    :initform (chronograph:iso-now)
    :accessor event-timestamp)
   (subject
    :initarg :subject
    :initform ""
    :accessor event-subject)
   (predicate
    :initarg :predicate
    :initform ""
    :accessor event-predicate)
   (object
    :initarg :object
    :initform ""
    :accessor event-object)))

(defun insert-event (timestamp subject predicate object)
  (push
   (make-instance 'event
                  :timestamp timestamp
                  :subject subject
                  :predicate predicate
                  :object object)
   *events*))

(defun add-event (subject predicate object)
  (push
   (make-instance 'event
                  :subject subject
                  :predicate predicate
                  :object object)
   *events*))

(defun future-events ()
  nil)
