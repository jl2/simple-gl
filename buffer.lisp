;; buffer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass buffer ()
  ((bo :initform 0 :initarg :bo)
   (pointer :initarg :pointer :accessor pointer)
   (target :initform :array-buffer :initarg :target :accessor target)
   (usage :initform :static-draw :initarg :usage :accessor usage)
   (stride :initform nil :initarg :stride)
   (free :initform t :initarg :free)
   (needs-rebind :initform t :initarg :needs-rebind :accessor needs-rebind)
   (needs-realloc :initform t :initarg :needs-realloc :accessor needs-realloc))
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
  ((block-index :initform 0 :initarg :block-index)
   (block-name :initarg :block-name)
   (bind-location :initform 0 :initarg :bind-location))
  (:documentation "A mutable buffer containing uniform values.."))


(defclass instance-buffer (attribute-buffer)
  ((attributes :initform '(("obj_transform" . :mat4)) :initarg :attributes))
  (:documentation "A mutable buffer containing instance data."))

(defgeneric compute-stride (buffer)
  (:documentation "Use the 'attributes' slot to compute the stride of the buffer data."))

(defgeneric associate-attributes (buffer program)
  (:documentation "Call gl:vertex-attrib-pointer and gl:enable-vertex-attrib-array as appropriate~
                  to associate attribute data with shader variables."))

(defmethod bind ((buffer buffer))
  (with-slots (bo target usage free pointer) buffer
    (when (= bo 0)
      (when (null pointer)
        (error "Cannot fill buffer from nil."))
      (setf bo (car (gl:gen-buffers 1)))
      (gl:bind-buffer target bo)
      (gl:buffer-data target usage pointer)
      (when (and free pointer)
        (free-gl-array pointer)
        (setf pointer nil))
      )))

(defmethod show-info ((object buffer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (next-ws (indent-whitespace (1+ indent))))
    (format t "~a~a~%" this-ws object)
    (show-slots next-ws object '(bo pointer target usage stride free))))

(defmethod show-info ((object attribute-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(attributes))))

(defmethod cleanup ((buffer buffer))
  (with-slots (bo target free pointer) buffer
    (when (and bo (> bo 0))
      (gl:bind-buffer target 0)
      (gl:delete-buffers (list bo))
      (setf bo 0)
      (when pointer
        (free-gl-array pointer))
      (setf pointer nil))))



(defmethod compute-stride ((buffer buffer))
  (with-slots (stride) buffer
    (when (null stride)
        (setf stride 1))
    stride))

(defmethod compute-stride ((buffer attribute-buffer))
  (with-slots (stride attributes) buffer
    (when (null stride)
      (setf stride
            (loop for (nil . type) in attributes
                  summing (glsl-byte-size type))))
    stride))


(defmethod associate-attributes ((buffer buffer) program)
  (declare (ignorable buffer program))
  t)


(defmethod associate-attributes ((buffer attribute-buffer) program)
  (with-slots (attributes) buffer
    (loop
          with stride = (compute-stride buffer)
          for offset = 0 then (+ offset byte-size)
          for (name . type) in attributes
          for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
          do
             (let ((entry-attrib (gl:get-attrib-location program name)))
               (when (>= entry-attrib 0)
                 (loop for i below vec4-size
                       for attrib-idx = (+ entry-attrib i)
                       do
                          (gl:vertex-attrib-pointer attrib-idx
                                                    comp-count
                                                    comp-type
                                                    :false
                                                    stride
                                                    (+ offset (* comp-count 4 i)))
                          (gl:enable-vertex-attrib-array attrib-idx)
                       )))))
  t)

(defmethod associate-attributes ((buffer instance-buffer) program)
  (with-slots (attributes) buffer
    (loop
          with stride = (compute-stride buffer)
          for offset = 0 then (+ offset byte-size)
          for (name . type) in attributes
          for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
          do
             (let ((entry-attrib (gl:get-attrib-location program name)))
               (when (>= entry-attrib 0)
                 (loop for i below vec4-size
                       for attrib-idx = (+ entry-attrib i)
                       do
                          (gl:vertex-attrib-pointer attrib-idx
                                                    (min comp-count 4)
                                                    comp-type
                                                    :false
                                                    stride
                                                    (+ offset (* 4 4 i)))
                          (gl:enable-vertex-attrib-array attrib-idx)
                          (%gl:vertex-attrib-divisor attrib-idx 1)
                       )))))
  t)

(defmethod reload ((buffer buffer))
  (bind buffer)
  (with-slots (pointer target) buffer
    (when (null pointer)
      (error "Cannot refill buffer from nil."))
    (gl:buffer-sub-data target pointer)))


(declaim (inline allocate-gl-array free-gl-array gl-fset gl-iset gl-get to-gl-float-array to-gl-array fill-pointer-offset))
(defun allocate-gl-array (type count)
  (declare (optimize (speed 3))
           (type fixnum count))
  (gl:alloc-gl-array type count))

(defun free-gl-array (array)
  (declare (optimize (speed 3))
           (type gl:gl-array array))
  (gl:free-gl-array array))

(defun gl-iset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type fixnum value)
           (type fixnum value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-fset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type single-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-dset (array idx value)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type double-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-get (array idx)
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type gl:gl-array array))
  (gl:glaref array idx))


(defun to-gl-array (gl-type size arr)
  "Create an OpenGL array of the specified type, initialized with the contents of arr."
  (declare (optimize (speed 3)))
  (let* ((gl-array (allocate-gl-array gl-type size)))
    (fill-pointer-offset arr gl-array 0)
    gl-array))


(defun fill-buffer (buffer data)
  (with-slots (pointer) buffer
    (fill-pointer-offset data pointer 0)))

(defgeneric fill-pointer-offset (data ptr offset)
  (:documentation "Low-level function for filling a C buffer using a pointer and offset."))

(defmethod fill-pointer-offset ((data vec2) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (+ 2 offset))

(defmethod fill-pointer-offset ((data vector) ptr offset)
  (loop for obj across data
        for off = offset then next-off
        for next-off = (fill-pointer-offset obj ptr off)
        finally (return next-off)))

(defmethod fill-pointer-offset ((data vec3) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (+ 3 offset))

(defmethod fill-pointer-offset ((data vec4) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (gl-fset ptr (+ 3 offset) (vw data))
  (+ 4 offset))

(defmethod fill-pointer-offset ((data mat3) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-fset ptr (+ off offset) d)
        finally (return (+ off offset))))

(defmethod fill-pointer-offset ((data mat4) ptr offset)
  (loop for off from 0
        for d across (marr (mtranspose data))
        do
           (gl-fset ptr (+ off offset) d)
        finally (return (+ off offset))))

(defmethod fill-pointer-offset ((data list) ptr offset)
  (loop for obj in data
        for off = offset then next-off
        for next-off = (fill-pointer-offset obj ptr off)
        finally (return next-off)))

(defmethod fill-pointer-offset ((data integer) ptr offset)
  (gl-iset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data real) ptr offset)
  (gl-fset ptr offset data)
  (1+ offset))
