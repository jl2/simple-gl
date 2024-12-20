;; opengl-object.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)


(defclass opengl-object ()
  ((vao :initform 0
        :type fixnum
        :accessor vao
        :documentation "The OpenGL VAO handle.")
   (name :initform "GL Object"
         :initarg :name
         :type string
         :documentation "The name of the object.")
   (styles :initform (list (cons :point-style (point-style)))
           :type (or null list)
           :accessor styles
           :initarg :styles
           :documentation "A list of styles that apply to this object.")
   (textures :initform nil
             :type (or null list)
             :accessor textures
             :initarg :textures
             :documentation "A list of textures used by this object.")
   (buffers :initform nil
            :type (or null list)
            :accessor buffers
            :initarg :buffers
            :documentation "A list of buffers used by this object.")
   (uniforms :initform nil
             :type (or null list)
             :accessor uniforms
             :initarg :uniforms
             :documentation "A list of uniforms used by this object.")
   (primitive-type :initform :triangles
                   :documentation "OpenGL primitive type (:triangles, :points, :lines, etc.)"))
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defmethod initialized-p ((object opengl-object))
  (not (zerop (vao object))))

(defun ensure-initialized (object)
  (when (not (initialized-p object))
    (initialize object)))

(defmethod show-info ((object opengl-object) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (declare (ignorable plus-plus-ws plus-ws))

    (format t "~aObject:~%" this-ws)
    (show-slots plus-ws object '(name vao styles textures primitive-type))

    (loop :for (name . style) :in (styles object) :do
      (format t "~a~a~%" this-ws name)
      (show-info style :indent (1+ indent)))

    (loop :for (name . buffer) :in (buffers object) :do
      (format t "~a~a~%" this-ws name)
      (show-info buffer :indent (1+ indent)))

    (loop :for (nil . uniform) :in (uniforms object) :do
      (show-info uniform :indent (1+ indent)))))

(defmethod handle-key ((object opengl-object) window key scancode action mod-keys)
  (declare (ignorable object window key scancode action mod-keys))
  nil)

(defmethod handle-resize ((object opengl-object) window width height)
  (declare (ignorable object window width height))
  nil)

(defmethod handle-click ((object opengl-object) window click-info)
  (declare (ignorable object window click-info))
  nil)

(defmethod handle-scroll ((object opengl-object) window cpos x-scroll y-scroll)
  (declare (ignorable object window cpos x-scroll y-scroll))
  nil)

#+spacenav
(defmethod handle-3d-mouse-event ((object opengl-object) (event sn:motion-event))
  (declare (ignorable object event))
    (with-slots (uniforms buffers textures) object
      (loop for buffer in buffers do
        (handle-3d-mouse-event (cdr buffer) event))
      (loop for texture in textures do
        (handle-3d-mouse-event texture event))
      (loop for uniform in uniforms do
        (handle-3d-mouse-event (cdr uniform) event)))
  nil)

(defmethod update ((object opengl-object) elapsed-seconds )
  (declare (ignorable object elapsed-seconds))
  (with-slots (textures styles buffers) object
    (let ((buffers (concatenate
                             'list
                             (loop :for tex :in textures
                                   :for updated = (update tex elapsed-seconds)
                                   :when updated
                                     :collect tex)
                             (loop :for (nil . style) :in styles
                                   :for updated = (update style elapsed-seconds)
                                   :when updated
                                     :return style)
                             (loop :for (nil . buffer) :in buffers
                                   :for updated = (update buffer elapsed-seconds)
                                   :when updated
                                     :collect buffer))))
      (when buffers
        (make-instance 'buffer-reloader
                       :buffers buffers)))))

(defun set-uniform (obj name value type &key (overwrite t))
  (with-slots (uniforms) obj
    (cond ((and overwrite
                (assoc name uniforms :test #'string=))
           (set-value (alexandria:assoc-value uniforms name :test #'string=) value type))
          ((null (assoc name uniforms :test #'string=))
           (push (cons name (make-instance 'uniform :name name
                                                    :type type
                                                    :value value))
                 uniforms)))))

(defun get-uniform (obj name)
  (with-slots (uniforms) obj
    (if-let (previous (assoc name uniforms :test #'string=))
      (cdr previous))))

(defmethod initialize :before ((object opengl-object) &key)
  (cleanup object))

(defmethod initialize ((object opengl-object) &key)
  (when (initialized-p object)
    (error "initialize called on object where vao != 0 ~a" object))
  (with-slots (vao styles initialized) object
    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (loop :for (nil . style) :in styles :do
      (build-style style))

    (initialize-buffers object)
    (initialize-uniforms object)
    (initialize-textures object)))

(defmethod reload-buffers ((object opengl-object))

  (with-slots (buffers) object
    (dolist (buffer buffers)
      (cleanup (cdr buffer)))
    (setf buffers nil)
    (initialize-buffers object)
    t))


(defmethod rebuild-style ((object opengl-object))
  (bind object)
  (with-slots (styles buffers uniforms) object
    (setf styles (loop
                   :for pair :in styles
                   :for (name . style) :in styles
                   :for new-style = (clone style)
                   :collect
                   (handler-case
                       (progn
                         (format t "Rebuilding style...")
                         (build-style new-style)
                         (format t "~%cleaning up~%")
                         (cleanup style)
                         (use-style new-style)
                         (cons name new-style))
                     (shader-error (condition)
                       (format t "Shader error: ~a~%" condition)
                       (format t "Not replacing style.~%")
                       pair))))
    (loop
      :for (nil . style) :in styles
      :for program = (program style) :do
        (use-style style)
        (loop :for (nil . buffer) :in buffers :do
          (bind buffer)
          (associate-attributes buffer (program style)))
        (loop :for (nil . uniform) :in uniforms :do
          (use-uniform uniform program))))
  t)

(defmethod needs-rebuild ((object opengl-object))
  (with-slots (styles) object
    (loop :for (nil . style) :in styles
          :when (needs-rebuild style)
            :collect style)))

(defmethod refill-textures ((object opengl-object))
  (bind object)
  (with-slots (textures buffers uniforms) object
    (loop :for texture :in textures :do
          (reload texture)))
  t)

(defmethod initialize-uniforms ((object opengl-object) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4)
  (set-uniform object "view_transform" (meye 4) :mat4)
  t)



(defmethod initialize-buffers ((object opengl-object) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))

  (set-buffer object :vertices
              (constant-attribute-buffer
               (list
                 (vec3 -1.0f0 -1.0f0 0.0f0)
                 (vec4 0.1f0 0.8f0 0.1f0 1.0f0)
                 (vec3 1.0f0 -1.0f0 0.0f0)
                 (vec4 0.1f0 0.8f0 0.1f0 1.0f0)
                 (vec3 0.0f0 1.0f0 0.0f0)
                 (vec4 0.1f0 0.8f0 0.1f0 1.0f0))
               (* 7 3)
               '(("in_position" . :vec3) ("in_color" . :vec4))))

  (set-buffer object
              :indices
              (constant-index-buffer 3)))

(defmethod initialize-textures ((object opengl-object) &key)
  (declare (ignorable object))
  (loop
    :for texture :in (textures object)
    :do
       (initialize texture)))

(defmethod cleanup ((object opengl-object))
  (with-slots (vao buffers textures uniforms styles) object

    (when (/= 0 vao)
      (gl:bind-vertex-array vao)

      (dolist (texture textures)
        (cleanup texture))

      (dolist (uniform uniforms)
        (cleanup (cdr uniform)))

      (dolist (buffer buffers)
        (cleanup (cdr buffer)))

      (dolist (style styles)
        (cleanup (cdr style)))

      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf buffers nil)
      (setf vao 0))))

(defmethod bind ((object opengl-object))
  (with-slots (vao buffers textures) object
    (declare (type fixnum vao))
    (when (zerop vao)
      (error "Trying to bind an uninitialized opengl-object!"))
    (gl:bind-vertex-array vao)))

(defmethod render ((object opengl-object))
  (with-slots (buffers uniforms primitive-type styles) object
    (bind object)
    (loop :for (nil . style) :in styles :do
      (use-style style)
      (loop :for (nil . uniform) :in uniforms :do
        (use-uniform uniform (program style)))
      (when (and (assoc-value buffers :vertices)
                 (assoc-value buffers :indices))
        (gl:draw-elements primitive-type
                          (gl:make-null-gl-array :unsigned-int)
                          :count (idx-count (assoc-value buffers :indices)))))))

(defun set-buffer (object buffer-name buffer)
  (declare (type opengl-object object)
           (type buffer buffer))
  (with-slots (buffers styles idx-count) object
    (if-let  ((location (assoc buffer-name buffers)))
      (progn
        (cleanup (cdr location))
        (rplacd location buffer))

      (push (cons buffer-name buffer) buffers))

    (bind buffer)

    (loop :for (nil . style) :in styles :do
      (associate-attributes buffer (program style)))))

(defun get-buffer (object buffer-name)
  (assoc-value (buffers object) buffer-name))

(defun add-style (object name new-style)
  (declare (type opengl-object object))
  (with-slots (styles) object
    (when (null (assoc name styles))
      (push (cons name new-style) styles))))

(defun add-texture (object texture)
  (declare (type opengl-object object)
           (type texture texture))
  (push texture (textures object)))
