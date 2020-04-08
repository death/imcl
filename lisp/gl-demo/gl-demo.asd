;;;; +----------------------------------------------------------------+
;;;; | IMCL                                                           |
;;;; +----------------------------------------------------------------+

(asdf:defsystem #:gl-demo
  :description "OpenGL demo code for IMCL"
  :author "death <github.com/death>"
  :serial t
  :depends-on ("imcl" "cl-opengl" "cl-glfw3" "3d-matrices")
  :components
  ((:file "gl-demo")))
