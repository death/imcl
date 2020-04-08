;;;; +----------------------------------------------------------------+
;;;; | IMCL                                                           |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:demo
  :description "Demo code for IMCL"
  :author "death <github.com/death>"
  :serial t
  :depends-on ("imcl" "vecto")
  :components
  ((:file "demo")))
