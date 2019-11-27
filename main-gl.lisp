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

(defvar *initial-vertex-shader*
  '("#version 310 es
     layout(location = 0) in vec3 aPos;
     void main() {
       gl_Position.xyz = aPos;
       gl_Position.w = 1.0;
     }"))

(defvar *initial-fragment-shader*
  '("#version 310 es
     precision mediump float;
     out vec3 color;
     void main() {
       color = vec3(1, 0, 0);
     }"))

(defclass gl-stuff ()
  ((vao :initform nil :accessor vao)
   (vbo :initform nil :accessor vbo)
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
                          (fragment-shader-strings *gl-stuff*))))
  ;; Clear buffer.
  (apply #'gl:clear-color (bg-color *gl-stuff*))
  (gl:clear :color-buffer-bit)
  ;; Draw
  (gl:use-program (program *gl-stuff*))
  (gl:bind-vertex-array (vao *gl-stuff*))
  ;; (gl:draw-arrays :triangles 0 3)
  (gl:polygon-mode :front-and-back :fill)
  (%gl:draw-elements :triangles 6 :unsigned-int 0)
  (gl:bind-vertex-array 0))

(defun gl-stuff-init-vao-tri ()
  (setf (vao *gl-stuff*) (first (gl:gen-vertex-arrays 1)))
  (gl:bind-vertex-array (vao *gl-stuff*))
  (setf (vbo *gl-stuff*) (first (gl:gen-buffers 1)))
  (gl:bind-buffer :array-buffer (vbo *gl-stuff*))
  (gl:with-gl-array (arr :float :count 9)
    (let ((verts #(-1.0 -1.0 0.0
                   +1.0 -1.0 0.0
                   0.0 +1.0 0.0)))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i))))
    (gl:buffer-data :array-buffer :static-draw arr))
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer)))

(defun gl-stuff-init-vao ()
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
    (gl:with-gl-array (arr :float :count 12)
      (let ((verts #(0.5 0.5 0.0
                     0.5 -0.5 0.0
                     -0.5 -0.5 0.0
                     -0.5 0.5 0.0)))
      (dotimes (i (length verts))
        (setf (gl:glaref arr i) (aref verts i))))
    (gl:buffer-data :array-buffer :static-draw arr))
    (gl:enable-vertex-attrib-array 0)
    (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))))

(defun gl-stuff-recreate-program ()
  (gl:delete-program (program *gl-stuff*))
  (setf (program *gl-stuff*) nil))

(defmethod (setf vertex-shader-strings) :after (new-shader (object gl-stuff))
  (declare (ignore new-shader))
  (gl-stuff-recreate-program))

(defmethod (setf fragment-shader-strings) :after (new-shader (object gl-stuff))
  (declare (ignore new-shader))
  (gl-stuff-recreate-program))

(defun gl-tick ()
  "GL-TICK runs on each iteration of the UI loop, before imgui
rendering."
  (gl-stuff-tick))
