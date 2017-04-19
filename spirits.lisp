;;;; spirits.lisp

(in-package #:spirits)

(defun past (&optional entry)
  (cond ((null entry)
         (mapcar #'entry-line *entries*)
         (length *entries*))
        (t (append *entries* `(,entry)))))

(defun last-n (&optional (lines 1))
  (subseq *entries* 0 lines))

(defun show-last-n (lines)
  (mapcar #'entry-line (last-n lines)))

(defun present (&optional text)
  "Either see the state of the present (todo/shopping lists, trades in play),
or create an entry to be placed in the immediate past"
  (cond ((null text)
         (trading:trade-status)
         (need)
         (todo))
        (t
         (push
          (make-instance 'entry
                         :text text
                         :timestamp (get-universal-time))
          *entries*)
         (save-entries))))

(defun need ()
  "Shopping list"
  (format t "~%~a~%" "Shopping list"))

(defclass todo-item ()
  ((timestamp
    :initarg timestamp
    :initform (chronograph:iso-now))))

(defun todo ()
  "Todo list"
  (format t "~%~a~%" "Todo list"))

(defun future (&optional event)
  "Either upcoming events or create a future event"
  (cond ((null event) (future-events))
        (t "Not implemented")))
