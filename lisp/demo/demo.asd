;;;; +----------------------------------------------------------------+
;;;; | IMGUI-ECL                                                      |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:demo
  :description "Demo code for imgui-ecl"
  :author "death <github.com/death>"
  :serial t
  :depends-on ("imgui" "vecto")
  :components
  ((:file "demo")))
