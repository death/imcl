;;;; +----------------------------------------------------------------+
;;;; | IMGUI-ECL                                                      |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:imgui
  :description "Lisp support code for imgui-ecl"
  :author "death <github.com/death>"
  :serial t
  :components
  ((:file "colors")
   (:file "ui")))
