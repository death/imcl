;;;; +----------------------------------------------------------------+
;;;; | IMGUI-ECL                                                      |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:gl-demo
  :description "OpenGL demo code for imgui-ecl"
  :author "death <github.com/death>"
  :serial t
  :depends-on ("imgui" "cl-opengl" "cl-glfw3" "3d-matrices")
  :components
  ((:file "gl-demo")))
