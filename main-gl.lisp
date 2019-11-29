(defun create-vertex-shader (string-list)
  (let ((id (gl:create-shader :vertex-shader)))
    (gl:shader-source id string-list)
    (gl:compile-shader id)
    (let ((result (gl:get-shader id :compile-status))
          (info-log-len (gl:get-shader id :info-log-length)))
      (when (plusp info-log-len)
        (format t "VERTEX SHADER: ~A~%" (gl:get-shader-info-log id))))
    id))

(defun create-fragment-shader (string-list)
  (let ((id (gl:create-shader :fragment-shader)))
    (gl:shader-source id string-list)
    (gl:compile-shader id)
    (let ((result (gl:get-shader id :compile-status))
          (info-log-len (gl:get-shader id :info-log-length)))
      (when (plusp info-log-len)
        (format t "FRAGMENT SHADER: ~A~%" (gl:get-shader-info-log id))))
    id))

(defun create-program (vertex-shader-strings fragment-shader-strings)
  (let ((v-id (create-vertex-shader vertex-shader-strings))
        (f-id (create-fragment-shader fragment-shader-strings))
        (p-id (gl:create-program)))
    (gl:attach-shader p-id v-id)
    (gl:attach-shader p-id f-id)
    (gl:link-program p-id)
    (let ((result (gl:get-program p-id :link-status))
          (info-log-len (gl:get-program p-id :info-log-length)))
      (when (plusp info-log-len)
        (format t "PROGRAM: ~A~%" (gl:get-program-info-log p-id))))
    (gl:detach-shader p-id v-id)
    (gl:detach-shader p-id f-id)
    (gl:delete-shader v-id)
    (gl:delete-shader f-id)
    p-id))

(defconstant deg-to-rad-factor
  (coerce (/ pi 180) 'single-float))

(defconstant rad-to-deg-factor
  (coerce (/ 180 pi) 'single-float))

(defun rad (x)
  (* x deg-to-rad-factor))

(defun deg (x)
  (* x rad-to-deg-factor))

(defvar *initial-vertex-shader*
  '("#version 310 es
     layout(location = 0) in vec3 aPos;
     layout(location = 1) in vec2 aTexCoord;
     out vec2 TexCoord;
     uniform mat4 transform;
     void main() {
       gl_Position = transform * vec4(aPos, 1.0);
       TexCoord = aTexCoord;
     }"))

(defvar *initial-fragment-shader*
  '("#version 310 es
     precision mediump float;
     out vec4 FragColor;
     in vec2 TexCoord;
     uniform sampler2D ourTexture;
     void main() {
       FragColor = texture(ourTexture, vec2(TexCoord.x, 1.0 - TexCoord.y));
     }"))

(defclass gl-stuff ()
  ((vao :initform nil :accessor vao)
   (vbo :initform nil :accessor vbo)
   (tex :initform nil :accessor tex)
   (program :initform nil :accessor program)
   (vertex-shader-strings :initform *initial-vertex-shader* :accessor vertex-shader-strings)
   (fragment-shader-strings :initform *initial-fragment-shader* :accessor fragment-shader-strings)
   (bg-color :initform (list 0.45 0.55 0.6 1.0) :accessor bg-color)))

(defvar *gl-stuff*
  (make-instance 'gl-stuff))

(defun gl-stuff-tick ()
  ;; Initialize VAO if necessary.
  (when (null (vao *gl-stuff*))
    (gl-stuff-init-vao))
  ;; Create program if necessary.
  (when (null (program *gl-stuff*))
    (setf (program *gl-stuff*)
          (create-program (vertex-shader-strings *gl-stuff*)
                          (fragment-shader-strings *gl-stuff*)))
    (gl:use-program (program *gl-stuff*))
    (let ((loc (gl:get-uniform-location (program *gl-stuff*) "transform"))
          (mat (3d-matrices:marr (3d-matrices:meye 4))))
      (gl:uniform-matrix loc 4 (vector mat))))
  ;; Clear buffer.
  (apply #'gl:clear-color (bg-color *gl-stuff*))
  (gl:clear :color-buffer-bit)
  ;; Draw
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:use-program (program *gl-stuff*))
  (gl:bind-vertex-array (vao *gl-stuff*))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (tex *gl-stuff*))
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (gl:bind-vertex-array 0))

(defun gl-stuff-init-vao ()
  (setf (tex *gl-stuff*) (load-texture "lisplogo_alien_256.png"))
  (gl:bind-texture :texture-2d (tex *gl-stuff*))
  (gl:tex-parameter :texture-2d :texture-min-filter :linear)
  (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
  (setf (vao *gl-stuff*) (first (gl:gen-vertex-arrays 1)))
  (gl:bind-vertex-array (vao *gl-stuff*))
  (let ((vbo (first (gl:gen-buffers 1)))
        (ebo (first (gl:gen-buffers 1))))
    (gl:bind-buffer :element-array-buffer ebo)
    (gl:with-gl-array (arr :unsigned-int :count 6)
      (setf (gl:glaref arr 0) 0)
      (setf (gl:glaref arr 1) 1)
      (setf (gl:glaref arr 2) 3)
      (setf (gl:glaref arr 3) 1)
      (setf (gl:glaref arr 4) 2)
      (setf (gl:glaref arr 5) 3)
      (gl:buffer-data :element-array-buffer :static-draw arr))
    (gl:bind-buffer :array-buffer vbo)
    (gl:with-gl-array (arr :float :count 20)
      (let ((verts #(0.5 0.5 0.0 1.0 1.0
                     0.5 -0.5 0.0 1.0 0.0
                     -0.5 -0.5 0.0 0.0 0.0
                     -0.5 0.5 0.0 0.0 1.0)))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i))))
    (gl:buffer-data :array-buffer :static-draw arr))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil (* 5 4) (cffi:null-pointer))
    (gl:enable-vertex-attrib-array 1)
    (gl:vertex-attrib-pointer 1 2 :float nil (* 5 4) (* 3 4))))

(defun recreate-program ()
  (gl:delete-program (program *gl-stuff*))
  (setf (program *gl-stuff*) nil))

(defun gl-tick ()
  "GL-TICK runs on each iteration of the UI loop, before imgui
rendering."
  (with-simple-restart (skip "Skip this tick")
    (gl-stuff-tick)))

(defclass transform-model ()
  ((translate :initform (list 0.0 0.0) :accessor tr-translate)
   (scale :initform (list 1.0) :accessor tr-scale)
   (rotate :initform (list 0.0) :accessor tr-rotate)))

(defvar *transform-model*
  (make-instance 'transform-model))

(defun show-transform (&optional (model *transform-model*))
  (window "Transform"
    (let ((update-matrix nil))
      (when (drag-float "Translate" (tr-translate model) 0.01)
        (setf update-matrix t))
      (when (drag-float "Scale" (tr-scale model) 0.01)
        (setf update-matrix t))
      (when (drag-float "Rotate" (tr-rotate model) 0.01)
        (setf update-matrix t))
      (when update-matrix
        (let* ((v-t (destructuring-bind (x y) (tr-translate model)
                      (3d-vectors:vec x y 0)))
               (v-s (destructuring-bind (x) (tr-scale model)
                      (3d-vectors:vec x x 1)))
               (v-r (first (tr-rotate model)))
               (loc (gl:get-uniform-location (program *gl-stuff*) "transform"))
               (mat (3d-matrices:marr
                     (3d-matrices:m*
                      (3d-matrices:mtranslation v-t)
                      (3d-matrices:mrotation 3d-vectors:+vz+ v-r)
                      (3d-matrices:mscaling v-s)))))
          (gl:uniform-matrix loc 4 (vector mat)))))))
