;; common.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(deftype point () 'vec3)

(deftype normal () 'vec3)

(deftype color () 'vec4)

(defclass mouse-click ()
  ((cursor-pos :initarg :cursor-pos)
   (button :initarg :button)
   (action :initarg :action)
   (mod-keys :initarg :mod-keys)
   (time :initarg :time))
  (:documentation "Mouse click information."))

(defclass viewer ()
  ((window
    :initform nil
    :documentation "The GLFW window for this viewer.")

   (viewer-mutex
    :initform (bt:make-lock "viewer-lock")
    :documentation "Mutex to synchronize multi-threaded access to this viewer.")

   (objects
    :initform nil
    :initarg :objects
    :type (or null cons)
    :accessor objects
    :documentation "The list of objects shown in this viewer.")

   (camera-position
    :initarg :camera-position
    :initform (vec3 0 0 1)
    :accessor camera-position
    :documentation "Camera location of the viewer.")

   (view-changed
    :initform t
    :documentation "t when the view has changed recently.")

   (aspect-ratio
    :initform 1.0
    :initarg :aspect-ratio
    :type real
    :accessor aspect-ratio
    :documentation "Aspect ratio of the view window.")

   (show-fps
    :initform nil
    :initarg :show-fps
    :type t
    :accessor show-fps
    :documentation "t to show FPS to *error-stream*.  nil to disable.")

   (desired-fps
    :initform 60
    :initarg :desired-fps
    :type fixnum
    :accessor desired-fps
    :documentation "Target FPS. The viewer should achieve this frame rate as long as render and update calls are fast enough.")

   (last-update-time
    :initform 0
    :documentation "Timestamp when the last update call finished")

   (seconds-between-updates
    :initform (/ 1 30)
    :initarg :seconds-between-updates
    :documentation "Ideal number of seconds between each update call.  Should be ~ (1 / desired-fps)")

   ;; Should cull-face, front-face, and blend be per-object?
   (cull-face
    :initform nil
    :initarg :cull-face
    :accessor cull-face
    :documentation "T to (gl:enable :cull-face), nil to (gl:disable :cull-face)")

   (front-face
    :initform :ccw
    :type (or :ccw :cw)
    :initarg :front-face
    :accessor front-face
    :documentation "Winding order of faces. :ccw or :cw.")

   (blend
    :initform t
    :initarg :blend
    :accessor blend
    :documentation "Enable (t) or disable (nil) blending.")

   (enable-update
    :initform t
    :initarg :enable-update
    :accessor enable-update
    :documentation "t to enable calls to (update), nil to disable. Used to pause or continue animations.")

   (background-color
    :initform (vec4 0.08f0 0.08f0 0.08f0 1.0)
    :initarg :background
    :accessor background-color
    :documentation "The background color.")

   (discarded-objects
    :initform nil
    :documentation "Objects that have been removed from the viewer, but still need to be cleaned up by OpenGL.")

   (initial-height
    :initform 800
    :initarg :initial-height
    :documentation "Requested initial window height.")

   (initial-width
    :initform 800
    :initarg :initial-width
    :documentation "Requested initial window width.")

   (previous-seconds
    :initform 0.0
    :documentation "Timestamp of previous render loop.")
   )
  (:documentation "A collection of objects and a viewport."))


(defgeneric add-object (viewer name object)
  (:documentation "Add an object with the given name to the viewer."))

(defgeneric rm-objcet (viewer name)
  (:documentation "Remove the named object from the viewer."))

(defgeneric replace-object (viewer name object)
  (:documentation "Replace the named object with the new object."))

(defgeneric view-matrix (viewer)
  (:documentation "Return the view transformation the viewer is using."))

(defgeneric rebuild-style (object)
  (:documentation "Return the view transformation."))

(defgeneric refill-textures (object)
  (:documentation "Return the view transformation."))

(defclass offscreen-viewer (viewer)
  ((fbo :initform nil)
   (should-close :initform nil)
   (output-directory :initform "/tmp/simple-gl-offscreen-render/"
                     :initarg :output-directory))
  (:documentation "A collection of objects and a viewport that renders PNG images to a directory."))

(defclass quaternion-viewer (viewer)
  ((radius :initform 100.0 :type real)
   (quaternion :initform (vec4 0.0 0.0 0.0 0.0) :initarg :quaternion :type vec4)
   (min-radius :initform 0.1 :initarg :min-radius :type real)
   (max-radius :initform 100.0 :initarg :max-radius :type real)
   (zoom-factor :initform 2000.0 :initarg :zoom-factor :type real)
   (rotation-factor :initform 2000.0 :initarg :rotation-factor :type real))
  (:documentation "A viewer with 3d mouse camera navigation."))

(defclass style ()
  ((enabled :initform t
            :initarg :enabled
            :accessor enabledp
            :documentation "Whether this shader is enabled or not.")
   (program :initform 0
            :accessor program
            :type fixnum
            :documentation "OpenGL program handle.")
   (shaders :initform nil
            :type (or null list)
            :accessor shaders
            :initarg :shaders
            :documentation "List of shaders for this style.")
   (poly-mode :initform :fill
              :accessor poly-mode
              :initarg :poly-mode
              :documentation "Polygon mode (:fill or :line) for this style.")))

(defclass texture ()
  ((tex-type :initform :texture-2d
             :initarg :type
             :documentation "OpenGL texture type (:texture-2d, :texture-1d, :texture-3d, etc.)")
   (textures :initform nil
             :type (or null list)
             :documentation "OpenGL texture handles.")
   (size :initform #(1 1)
         :initarg :size
         :type sequence
         :documentation "n-dimensional array specifying the size of the texture.")
   (parameters :initform '((:texture-wrap-s . :repeat)
                           (:texture-wrap-t . :repeat)
                           (:texture-base-level . 0)
                           (:texture-max-level . 8)
                           (:texture-min-filter . :linear-mipmap-linear)
                           (:texture-mag-filter . :linear))
               :documentation "OpenGL texture parameters.")))

;; TODO: Consider creating float-uniform, mat4-uniform, etc. subclasses that call the
;; correct gl:uniform* functions, and remove the big cond in use-uniform.
(defclass uniform ()
  ((name :initarg :name
         :type string
         :documentation "Uniform variable name.")
   (type :initarg :type
         :documentation "Uniform variable type.")
   (value :initarg :value
          :initform nil
          :type t
          :documentation "Current uniform value.")
   (modified :initform t
             :documentation "Whether the uniform's current value has been sent to OpenGL."))
  (:documentation "A uniform variable parameter to a shader."))

(define-condition shader-error (error)
  ((status :initarg :status
           :reader shader-compile-status
           :documentation "The compile status.")
   (object :initarg :object
           :reader shader-object
           :documentation "The shader object that caused the shader-error.")
   (info-log :initarg :info-log
             :reader shader-compile-info-log
             :documentation "The compile info log text from OpenGL.")))

(defclass gl-shader ()
  ((shader :initform 0
           :type fixnum
           :accessor shader
           :documentation "The OpenGL shader handle object.")
   (shader-type :initarg :shader-type
                :documentation "The type of shader."))

  (:documentation "An opengl shader class."))

(defgeneric get-source (shader)
  (:documentation "Return shader source code as a string."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defclass gl-file-shader (gl-shader)
  ((source-file :initarg :source-file
                :type (or pathname string)
                :accessor source-file
                :documentation "The filename of the OpenGL shader file."))
  (:documentation "An OpenGL shader whose source code is stored in a file."))

(defclass 3d-mouse-nav-viewer (viewer)
  ((radius :initform 120.0
           :type real
           :documentation "The distance between the camera and the target.")

   (θ :initform (/ pi 2)
      :type real
      :documentation "")
   (θ-direction :initform 1.0
                :type real
                :documentation "")
   (φ :initform (/ pi 6)
      :type real
      :documentation "")
   (φ-direction :initform 1.0
                :type real
                :documentation "")
   (target :initform (vec3 0.0 0.0 0.0)
           :initarg :target
           :type vec3
           :documentation "")
   (up :initform (vec3 0 1 0)
       :initarg :up
       :type vec3
       :documentation "")
   (min-radius :initform 0.1
               :initarg :min-radius
               :type real
               :documentation "")
   (max-radius :initform 100.0
               :initarg :max-radius
               :type real
               :documentation "")
   (zoom-factor :initform 2000.0
                :initarg :zoom-factor
                :type real
                :documentation "")
   (rotation-factor :initform 2000.0
                    :initarg :rotation-factor
                    :type real
                    :documentation ""))
  (:documentation "A viewer with keyboard and 3d mouse camera rotation around a target point."))


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

(defclass instanced-opengl-object (opengl-object)
  ((styles :initform (list (cons :instanced-point-style (point-style-instanced)))
           :type (or null list)
           :accessor styles
           :initarg :styles)
   (max-instances :initform 100
                  :initarg :max-instances
                  :documentation "Maximum number of instances.  This is used to allocate space ahead of time.")
   (instance-count :initform 0
                   :initarg :instance-count
                   :documentation "Current instance count."))
  (:documentation "A pool of identical objects using OpenGL instancing.  Instance data is passed in buffers."))

(defclass quad (opengl-object)
  ()
  (:documentation "A quad."))


(defclass stl-file (instanced-opengl-object)
  ((file-name :initarg :file-name :type (or string path))
   (tri-count :initform 0 :type (unsigned-byte 32))))


(defclass buffer ()
  ((bo :initform 0 :initarg :bo)
   (pointer :initarg :pointer :accessor pointer)
   (target :initform :array-buffer :initarg :target :accessor target)
   (usage :initform :static-draw :initarg :usage :accessor usage)
   (stride :initform nil :initarg :stride)
   (free :initform t :initarg :free))
  (:documentation "An OpenGL buffer object."))

(defclass attribute-buffer (buffer)
  ((stride :initform nil)
   (target :initform :array-buffer :initarg :target)
   (attributes :initform '(("in_position" . :vec3) ("in_color" . :vec4))
               :initarg :attributes))
  (:documentation "An OpenGL vertex buffer containing (mutable) vertex data that is passed into a shader."))

(defclass index-buffer (buffer)
  ((target :initform :element-array-buffer)
   (idx-count :initform 0 :initarg :idx-count :accessor idx-count)
   (stride :initform 1))
  (:documentation "A GL_ELEMENT_ARRAY buffer containing integer indices for drawing."))

(defclass uniform-buffer (buffer)
  ((target :initform :uniform-buffer :initarg :target)
   (block-index :initform 0 :initarg :block-index)
   (block-name :initarg :block-name)
   (bind-location :initform 0 :initarg :bind-location))
  (:documentation "A mutable buffer containing uniform values.."))


(defclass instance-buffer (attribute-buffer)
  ((attributes :initform '(("obj_transform" . :mat4)) :initarg :attributes)
   (divisor :initform 1 :type fixnum :initarg :divisor))
  (:documentation "A mutable buffer containing instance data."))

(defgeneric compute-stride (buffer)
  (:documentation "Use the 'attributes' slot to compute the stride of the buffer data."))

(defgeneric associate-attributes (buffer program)
  (:documentation "Call gl:vertex-attrib-pointer and gl:enable-vertex-attrib-array as appropriate~
                  to associate attribute data with shader variables."))
(defgeneric fill-pointer-offset (data ptr offset)
  (:documentation "Low-level function for filling an OpenGL buffer using a pointer and offset."))


(defgeneric use-uniform (uniform program)
  (:documentation "Pass the uniform's value into the OpenGL program."))

(defgeneric get-value (uniform)
  (:documentation "Set a generic's value and optionally type."))

(defgeneric set-value (uniform new-value &optional new-type)
  (:documentation "Set a generic's value and optionally type."))



(defgeneric reset-view-safe (viewer)
  (:documentation "Reset view to its initial conditions."))

(defgeneric use-style (style)
  (:documentation "Apply style settings."))



(defgeneric fill-texture (obj))

(defgeneric initialize (object &key)
  (:documentation "Initialize an OpenGL object.  Default implementation calls the initialize-* methods."))

(defgeneric initialize-shaders (object &key)
  (:documentation "Initialize the shaders associated with an OpenGL object."))

(defgeneric initialize-buffers (object &key)
  (:documentation "Initialize the buffers associated with an OpenGL object."))

(defgeneric initialize-uniforms (object &key)
  (:documentation "Initialize the uniforms associated with an OpenGL object."))

(defgeneric initialize-textures (object &key)
  (:documentation "Initialize the textures associated with object."))

(defgeneric render (object)
  (:documentation "Render an OpenGL object."))

(defgeneric bind (object)
  (:documentation "Bind buffers, textures, etc. for an object."))

(defgeneric reload (object)
  (:documentation "Copy new data for object to OpenGL."))

(defgeneric update (object elapsed-seconds)
  (:documentation "Called on an object before rendering, to update for the next animation frame."))

(defgeneric cleanup (object)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(declaim (inline handle-key handle-resize handle-click handle-scroll handle-3d-mouse-event update))
;; Input handlers
(defgeneric handle-key (object window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (object window click-info)
  (:documentation "Handle a mouse click."))

(defgeneric handle-scroll (object window cpos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-resize (object window width height)
  (:documentation "Handle window resize."))

(defgeneric initialized-p (object)
  (:documentation "Returns nil if object is not initialized, non-nil otherwise."))

(defgeneric build-style (object)
  (:documentation "Bind the correct VAO and build object's shader programs."))


(defun show-slots (white-space object slots)
  "Print the specified slots of object, one per line, indenting with white-space on each line.~
   Use 'nil' slot name to print an empty line to separate values."
  (dolist (slot slots)
    (cond ((null slot)
           (format t "~%"))
          (t
           (format t "~a~a: ~a~%" white-space slot (slot-value object slot))))))

(defgeneric show-info (object &key indent)
  (:documentation "Show OpenGL information for an object."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))


#+spacenav
(defgeneric handle-3d-mouse-event (object event)
  (:documentation "Handle a spacenav 3D mouse event."))

#+spacenav
(declaim (inline handle-3d-mouse-event))
#+spacenav
(defmethod handle-3d-mouse-event ((object t) (event t))
  (declare (ignorable object event))
  nil)
