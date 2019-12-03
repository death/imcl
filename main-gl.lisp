;; Utilities

(defconstant deg-to-rad-factor
  (coerce (/ pi 180) 'single-float))

(defconstant rad-to-deg-factor
  (coerce (/ 180 pi) 'single-float))

(defun rad (x)
  (* x deg-to-rad-factor))

(defun deg (x)
  (* x rad-to-deg-factor))

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

(defun set-matrix (program name mat)
  (let ((loc (gl:get-uniform-location program name))
        (arr (3d-matrices:marr mat)))
    (gl:uniform-matrix loc 4 (vector arr))))

;; Scene

(defclass scene ()
  ((alien :initform nil :accessor scene-alien)
   (bg-color :initform (list 0.45 0.55 0.6 1.0) :accessor bg-color)))

(defvar *scene*
  (make-instance 'scene))

(defun scene-tick ()
  (apply #'gl:clear-color (bg-color *scene*))
  (gl:clear :color-buffer-bit)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (unless (scene-alien *scene*)
    (setf (scene-alien *scene*) (make-alien)))
  (draw (scene-alien *scene*)))

(defun gl-tick ()
  "GL-TICK runs on each iteration of the UI loop, before imgui
rendering."
  (with-simple-restart (skip "Skip this tick")
    (scene-tick)))

;; Alien

(defvar *alien-vertex-shader*
  '("#version 310 es
     layout(location = 0) in vec3 aPos;
     layout(location = 1) in vec2 aTexCoord;
     out vec2 TexCoord;
     uniform mat4 model;
     uniform mat4 view;
     uniform mat4 projection;
     void main() {
       gl_Position = projection * view * model * vec4(aPos, 1.0);
       TexCoord = aTexCoord;
     }"))

(defvar *alien-fragment-shader*
  '("#version 310 es
     precision mediump float;
     out vec4 FragColor;
     in vec2 TexCoord;
     uniform sampler2D ourTexture;
     void main() {
       FragColor = texture(ourTexture, vec2(TexCoord.x, 1.0 - TexCoord.y));
     }"))

(defclass alien ()
  ((vao :initarg :vao :accessor vao)
   (tex :initarg :tex :accessor tex)
   (program :initarg :program :accessor program)
   (entities :initform '() :accessor entities)))

(defclass alien-entity ()
  ((model-mat :initarg :model-mat :accessor model-mat))
  (:default-initargs :model-mat (3d-matrices:meye 4)))

(defun make-alien ()
  (let ((tex (load-texture "lisplogo_alien_256.png"))
        (vao (first (gl:gen-vertex-arrays 1)))
        (program (create-program *alien-vertex-shader*
                                 *alien-fragment-shader*)))
    (gl:use-program program)
    (set-matrix program "model" (3d-matrices:meye 4))
    (set-matrix program "view" (3d-matrices:meye 4))
    (set-matrix program "projection" (3d-matrices:meye 4))
    (gl:bind-texture :texture-2d tex)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:bind-vertex-array vao)
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
        (let ((verts #( 0.5  0.5 0.0 1.0 1.0
                       0.5 -0.5 0.0 1.0 0.0
                       -0.5 -0.5 0.0 0.0 0.0
                       -0.5  0.5 0.0 0.0 1.0)))
          (dotimes (i (length verts))
            (setf (gl:glaref arr i) (aref verts i))))
        (gl:buffer-data :array-buffer :static-draw arr))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil (* 5 4) (cffi:null-pointer))
      (gl:enable-vertex-attrib-array 1)
      (gl:vertex-attrib-pointer 1 2 :float nil (* 5 4) (* 3 4)))
    (make-instance 'alien :vao vao :tex tex :program program)))

(defmethod draw ((alien alien))
  (gl:use-program (program alien))
  (gl:bind-vertex-array (vao alien))
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (tex alien))
  (dolist (entity (entities alien))
    (set-matrix (program alien) "model" (model-mat entity))
    (%gl:draw-elements :triangles 6 :unsigned-int 0))
  (gl:bind-vertex-array 0))

(defmethod print-object ((alien alien-entity) stream)
  (print-unreadable-object (alien stream :type t :identity t))
  stream)

;; Transform window

(defclass transform-model ()
  ((transformables :initform '() :accessor tr-transformables)
   (current :initform nil :accessor tr-current)))

(defclass transformable ()
  ((translate :initform (list 0.0 0.0) :accessor tr-translate)
   (scale :initform (list 1.0) :accessor tr-scale)
   (rotate :initform (list 0.0) :accessor tr-rotate)
   (entity :initarg :entity :accessor tr-entity)
   (remove-function :initarg :remove-function :accessor tr-remove-function)))

(defvar *transform-model*
  (make-instance 'transform-model))

(defun show-transform (&optional (model *transform-model*))
  (window "Transform"
    (let ((alien (scene-alien *scene*)))
      (when alien
        (when (button "Add alien")
          (let* ((entity (make-instance 'alien-entity))
                 (tr (make-instance 'transformable
                                    :entity entity
                                    :remove-function
                                    (lambda ()
                                      (setf (entities alien)
                                            (remove entity (entities alien)))))))
            (push entity (entities alien))
            (push tr (tr-transformables model))
            (setf (tr-current model) tr)))))
    (when (tr-transformables model)
      (list-box ("Transformables" (length (tr-transformables model)) 5)
        (let ((id 0))
          (dolist (tr (tr-transformables model))
            (with-id id
              (when (selectable (format nil "~S" (tr-entity tr))
                                (eq tr (tr-current model)))
                (setf (tr-current model) tr)))
            (incf id)))))
    (let ((tr (tr-current model)))
      (when tr
        (let ((update-matrix nil))
          (when (drag-float "Translate" (tr-translate tr) 0.01)
            (setf update-matrix t))
          (when (drag-float "Scale" (tr-scale tr) 0.01)
            (setf update-matrix t))
          (when (drag-float "Rotate" (tr-rotate tr) 0.01)
            (setf update-matrix t))
          (when update-matrix
            (let* ((v-t (destructuring-bind (x y) (tr-translate tr)
                          (3d-vectors:vec x y 0)))
                   (v-s (destructuring-bind (x) (tr-scale tr)
                          (3d-vectors:vec x x 1)))
                   (v-r (first (tr-rotate tr)))
                   (mat (3d-matrices:m*
                         (3d-matrices:mtranslation v-t)
                         (3d-matrices:mrotation 3d-vectors:+vz+ v-r)
                         (3d-matrices:mscaling v-s))))
              (setf (model-mat (tr-entity tr)) mat)))
          (when (button "Remove")
            (funcall (tr-remove-function tr))
            (setf (tr-transformables model)
                  (remove tr (tr-transformables model)))
            (setf (tr-current model)
                  (first (tr-transformables model)))))))))

(defvar *gl-menu-items*
  '(("Transform" show-transform)))

(defun change-main-menu ()
  (setf *main-menu*
        (mapcar (lambda (entry)
                  (if (equal (car entry) "OpenGL")
                      (cons "OpenGL" *gl-menu-items*)
                      entry))
                *main-menu*)))

(change-main-menu)
