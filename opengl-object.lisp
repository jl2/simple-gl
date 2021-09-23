;; opengl-object.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(declaim (inline handle-key handle-resize handle-click handle-scroll handle-3d-mouse-event update))

(defclass opengl-object ()
  ((vao :initform 0 :type fixnum)
   (name :initform "GL Object" :initarg :name)
   (style :initform (point-style)
          :type style
          :accessor style
          :initarg :style)
   (textures :initform nil
             :type (or null list)
             :accessor textures
             :initarg :textures)
   (buffers :initform nil
            :type (or null list)
            :accessor buffers
            :initarg :buffers)
   (uniforms :initform nil
             :type (or null list)
             :accessor uniforms
             :initarg :uniforms)
   (primitive-type :initform :triangles)
   )
  (:documentation "Base class for all objects that can be rendered in a scene."))

(defclass instanced-opengl-object (opengl-object)
  ((instance-count :initform 1 :initarg :instance-count)))

(defgeneric build-style (object)
  (:documentation "Build this object's shader programs.  Binding correct VAO is handled by before and after methods."))


(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))

(defmethod show-info ((object opengl-object) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (declare (ignorable plus-plus-ws plus-ws))
    (format t "~aObject:~%" this-ws)
    (show-slots plus-ws object '(name vao style textures))

    (format t "~a~a~%" this-ws (name (style object)))
    (show-info (style object) :indent (1+ indent))
    (dolist (buffer (buffers object))
      (format t "~a~a~%" this-ws (car buffer))
      (show-info (cdr buffer) :indent (1+ indent)))
    (dolist (uniform (uniforms object))
      (show-info (cdr uniform) :indent (1+ indent)))))

(defmethod show-info ((object instanced-opengl-object) &key (indent 0))
  (call-next-method)
  (let ((plus-ws (indent-whitespace (+ 1 indent))))
    (show-slots plus-ws object '(instance-count))))

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
    (with-slots (uniforms buffers textures style) object
      (loop for buffer in buffers do
        (handle-3d-mouse-event (cdr buffer) event))
      (loop for texture in textures do
        (handle-3d-mouse-event texture event))
      (loop for uniform in uniforms do
        (handle-3d-mouse-event (cdr uniform) event)))
  nil)

(defmethod update ((object opengl-object) elapsed-seconds )
  (declare (ignorable object elapsed-seconds))
  nil)

(defun set-uniform (obj name value type &key (overwrite t))
  (with-slots (uniforms) obj
    (if-let (previous (assoc name uniforms :test #'string=))
      (when overwrite
        (set-value (cdr previous) value type))
      (push (cons name (make-instance 'uniform :name name
                                               :type type
                                               :value value))
            uniforms))))

(defun get-uniform (obj name)
  (with-slots (uniforms) obj
    (if-let (previous (assoc name uniforms :test #'string=))
      (cdr previous))))

(defmethod initialize :before ((object opengl-object) &key)
  (cleanup object))

(defmethod initialize ((object opengl-object) &key)
  (with-slots (vao) object
    (when (/= 0 vao)
      (error "initialize called on object where vao != 0 ~a" object))

    (setf vao (gl:gen-vertex-array))
    (gl:bind-vertex-array vao)

    (build-style object)

    (initialize-buffers object)
    (initialize-uniforms object)
    (initialize-textures object)))

(defmethod build-style ((object opengl-object))
  (with-slots (style buffers) object
    (build-style style)))

(defmethod rebuild-style ((object opengl-object))
  (bind object)
  (with-slots (style buffers uniforms) object
    (cleanup style)
    (build-style style)
    (use-style style)
    (loop for buffer in buffers do
      (bind (cdr buffer))
      (associate-attributes (cdr buffer) (program style)))
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))
    ))

(defmethod initialize-uniforms ((object opengl-object) &key)
  (declare (ignorable object))
  t)

(defmethod initialize-buffers ((object instanced-opengl-object) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (set-buffer object
              :vertices
              (make-instance
               'attribute-buffer
               :pointer (to-gl-array
                         :float
                         9
                         (list -0.5f0 -0.5f0 0.0f0

                               0.5f0 -0.5f0 0.0f0

                               0.0f0 (- (sqrt (- 1.0f0 (* 0.5f0 0.5f0))) 0.5f0)  0.0f0))
               :stride nil
               :attributes '(("in_position" . :vec3))
               :usage :static-draw
               :free t))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 3
               :pointer (to-gl-array :unsigned-int 3 #(0 1 2))
               :stride nil
               :usage :static-draw
               :free t))
  (set-buffer object
              :obj-transform (make-instance
                          'instance-buffer
                          :pointer (to-gl-array :float 32 (list
                                                           (meye 4)
                                                            (mtranslation (vec3 0.0 0.5 0.5))))
                          :stride nil
                          :attributes '(("obj_transform" . :mat4))
                          :usage :static-draw
                          :free nil))
  (set-buffer object
              :obj-color (make-instance
                          'instance-buffer
                          :pointer (to-gl-array :float 8
                                                (list (vec4 0.1 0.8 0.1 1.0)
                                                           (vec4 0.1 0.1 0.8 1.0))
                                                )
                          :stride nil
                          :attributes '(("in_color" . :vec4))
                          :usage :static-draw
                          :free nil))
  (setf (slot-value object 'instance-count) 2))


(defmethod initialize-textures ((object opengl-object) &key)
  (declare (ignorable object))
  (loop for texture in (textures object) do
        (initialize texture)))

(defmethod cleanup ((object opengl-object))
  (with-slots (vao buffers textures uniforms style) object

    (when (/= 0 vao)
      (gl:bind-vertex-array vao)

      (when textures
        (dolist (texture textures)
          (cleanup texture)))

      (when uniforms
        (dolist (uniform uniforms)
          (cleanup (cdr uniform))))

      (when buffers
        (dolist (buffer buffers)
          (cleanup (cdr buffer))))

      (when style
        (cleanup style))

      (gl:bind-vertex-array 0)
      (gl:delete-vertex-arrays (list vao))
      (setf vao 0))))

(defmethod bind ((object opengl-object))
  (with-slots (vao buffers textures) object
    (if (= vao 0)
        (error "Trying to bind an uninitialized opengl-object!")
        (gl:bind-vertex-array vao))
    ;; (dolist (buffer buffers)
    ;;   (bind (cdr buffer)))
    ;; (dolist (texture textures)
    ;;   (bind texture))
    ))

(defmethod render ((object opengl-object))
  (with-slots (buffers uniforms primitive-type style) object
    (bind object)
    (use-style style)

    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))

    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count (idx-count (assoc-value buffers :indices)))))

(defmethod render ((object instanced-opengl-object))
  (with-slots (buffers uniforms primitive-type instance-count style) object
    (bind object)
    (use-style style)
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))


    (when (> instance-count 0)
      (gl:draw-elements-instanced  primitive-type
                                   (gl:make-null-gl-array :unsigned-int)
                                   instance-count
                                   :count (idx-count (assoc-value buffers :indices))))))

(defun set-buffer (object buffer-name buffer)
  (declare (type opengl-object object)
           (type buffer buffer))
  (with-slots (buffers style idx-count) object
    (if-let  ((location (assoc buffer-name buffers)))
      (progn
        (cleanup (cdr location))
        (rplacd location buffer))

      (push (cons buffer-name buffer) buffers))

    (bind buffer)
    (associate-attributes buffer (program style))))

(defun set-style (object new-style)
  (declare (type opengl-object object)
           (type style new-style))
  (with-slots (style) object
    (when style
      (cleanup style))
    (setf style new-style)))

(defun get-buffer (object buffer-name)
  (assoc-value (buffers object) buffer-name))

(defun add-texture (object texture)
  (declare (type opengl-object object)
           (type texture texture))
  (push texture (textures object)))
