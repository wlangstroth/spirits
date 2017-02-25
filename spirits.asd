;;;; spirits.asd

(asdf:defsystem #:spirits
  :description "Past, present and future"
  :author "Will Langstroth <will@langstroth.com>"
  :license "MIT"
  :serial t
  :depends-on (#:cl-utilities
               #:cl-json)
  :components ((:file "package")
               (:file "spirits")))
