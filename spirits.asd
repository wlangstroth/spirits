;;;; spirits.asd

(asdf:defsystem #:spirits
  :description "Past, present and future"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-utilities
               #:passage
               #:trading)
  :components ((:file "package")
               (:file "entries")
               (:file "events")
               (:file "items")
               (:file "spirits")))
