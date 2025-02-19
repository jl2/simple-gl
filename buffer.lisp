;; buffer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(declaim (inline
          gl-fset gl-iset gl-dset
          gl-get
          to-gl-array))

(defclass buffer ()
  ((bo
    :initform 0
    :initarg :bo
    :type fixnum
    :documentation "The OpenGL buffer ID.")
   (pointer
    :initarg :pointer
    :accessor pointer
    :documentation "The pointer to memory that is passed to OpenGL.")
   (target
    :initform :array-buffer
    :initarg :target
    :type (or :array-buffer :element-array-buffer)
    :accessor target
    :documentation "The OpenGL target for the buffer, :array-buffer or :element-array-buffer.")
   (usage
    :initform :static-draw
    :initarg :usage
    :type (or :static-draw :dynamic-draw :stream)
    :accessor usage
    :documentation "The intended usage of the buffer.  :static-draw or :dynamic-draw or :stream")
   (stride
    :initform nil
    :initarg :stride
    :type (or null fixnum)
    :documentation "The stride (number of floats) between each buffer entry.")
   (free
    :initform t
    :initarg :free
    :type boolean
    :documentation "Whether or not to free the memory used to pass data to OpenGL.  If free is non-nil,
 pointer will be nil after the buffer data is sent to OpenGL."))
  (:documentation "An OpenGL buffer object."))


(defclass attribute-buffer (buffer)
  ((stride
    :initform nil)
   (target
    :initform :array-buffer
    :initarg :target)
   (attributes
    :initform '(("in_position" . :vec3) ("in_color" . :vec4))
    :initarg :attributes
    :documentation "A list of attribute name/type pairs describing the buffer's data."))
  (:documentation "An OpenGL vertex buffer containing (mutable) vertex data that is passed into a shader."))


(defclass index-buffer (buffer)
  ((target
    :initform :element-array-buffer)
   (idx-count
    :initform 0
    :initarg :idx-count
    :accessor idx-count
    :documentation "The number of indices.")
   (stride
    :initform 1))
  (:documentation "A GL_ELEMENT_ARRAY buffer containing integer indices for drawing."))


(defclass uniform-buffer (buffer)
  ((target
    :initform :uniform-buffer
    :initarg :target)
   (block-index
    :initform 0
    :initarg :block-index)
   (block-name
    :initarg :block-name)
   (bind-location
    :initform 0
    :initarg :bind-location))
  (:documentation "A mutable buffer containing uniform values."))


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


(defun constant-attribute-buffer (data float-count attributes &key (free t) (usage :static-draw))
  (make-instance 'attribute-buffer
                 :pointer (to-gl-array :float
                                       float-count
                                       data)
                 :stride nil
                 :attributes attributes
                 :usage usage
                 :free free))

(defun constant-index-buffer (count &key
                                      (data (loop :for i :below count
                                                  :collecting i))
                                      (free t))
  (make-instance 'index-buffer
                 :idx-count count
                 :pointer (to-gl-array :unsigned-int
                                       count
                                       data)
                 :free free))

(defun constant-instance-buffer (data float-count attributes &key
                                                               (divisor 1)
                                                               (free t)
                                                               (usage :static-draw))
  (make-instance 'instance-buffer
                 :pointer (to-gl-array :float
                                       float-count
                                       (ensure-list data))
                 :stride nil
                 :divisor divisor
                 :attributes attributes
                 :usage usage
                 :free free))

(defmethod bind ((buffer buffer))
  (with-slots (bo target usage free pointer) buffer
    (cond
      ((null pointer)
       (error "Cannot fill buffer from nil."))
      ((= bo 0)
       (setf bo (car (gl:gen-buffers 1)))
       (gl:bind-buffer target bo)
       ;;(format t "Calling buffer-data target ~a ~a~%" target usage)
       ;; (show-gl-array pointer)
       (gl:buffer-data target usage pointer)
       (when (and free pointer)
         (gl:free-gl-array pointer)
         (setf pointer nil)))
      (t
       (gl:bind-buffer target bo)))))

(defmethod show-info ((object buffer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (next-ws (indent-whitespace (1+ indent))))
    (format t "~a~a~%" this-ws object)
    (show-slots next-ws object '(bo pointer target usage stride free))
    (with-slots (pointer) object
      (when pointer
        (show-gl-array pointer)))))

(defmethod show-info ((object attribute-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(attributes))))

(defmethod show-info ((object instance-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(divisor))))

(defmethod show-info ((object index-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(idx-count))))

(defmethod show-info ((object uniform-buffer) &key (indent 0))
  (call-next-method)
  (let ((next-ws (indent-whitespace (1+ indent))))
    (show-slots next-ws object '(block-name bind-location block-idx))))

(defmethod cleanup ((buffer buffer))
  (with-slots (bo target free pointer) buffer
    (when (and bo (> bo 0))
      (gl:bind-buffer target 0)
      (gl:delete-buffers (list bo))
      (setf bo 0)
      (when pointer
        (gl:free-gl-array pointer))
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
            (loop
              :for (nil . type) :in attributes
              :summing (glsl-byte-size type))))
    stride))


(defmethod associate-attributes ((buffer buffer) program)
  (declare (ignorable buffer program))
  t)


(defmethod associate-attributes ((buffer attribute-buffer) program)
  (with-slots (attributes) buffer
    (loop
      :with stride fixnum = (compute-stride buffer)
      :for offset fixnum = 0 :then (+ offset byte-size)
      :for (name . type) :in attributes
      :for (comp-type comp-count byte-size vec4-size) :of-type (t fixnum fixnum fixnum) = (glsl-type-info type)
      :for this-comp-count fixnum = (min comp-count 4)
      :for entry-attrib fixnum = (gl:get-attrib-location program name)
      :when (>= entry-attrib 0) :do
        (loop
          :for i fixnum :below vec4-size
          :for attrib-idx fixnum = (+ entry-attrib i)
          :for this-offset fixnum = (+ offset (* this-comp-count 4 i))
          :for is-int = (eq comp-type :int)
          :for is-double = (eq comp-type :double)
          :when is-int :do
            (gl:vertex-attrib-ipointer attrib-idx
                                       this-comp-count
                                       comp-type
                                       stride
                                       this-offset)
          :when is-double :do
            (%gl:vertex-attrib-lpointer attrib-idx
                                        this-comp-count
                                        comp-type
                                        stride
                                        this-offset)
          :when (and (not is-double) (not is-int)) :do
            (gl:vertex-attrib-pointer attrib-idx
                                      this-comp-count
                                      comp-type
                                      nil
                                      stride
                                      this-offset)
          :do
             (gl:enable-vertex-attrib-array attrib-idx))))
  t)

(defmethod associate-attributes ((buffer instance-buffer) program)
  (with-slots (attributes divisor) buffer
    (loop
      :with stride fixnum = (compute-stride buffer)
      :for offset fixnum = 0 :then (+ offset byte-size)
      :for (name . type) :in attributes
      :for (comp-type comp-count byte-size vec4-size) :of-type (t fixnum fixnum fixnum) = (glsl-type-info type)
      :for this-comp-count fixnum = (min comp-count 4)
      :for entry-attrib fixnum = (gl:get-attrib-location program name)
      :when (>= entry-attrib 0) :do
        (loop
          :for i fixnum :below vec4-size
          :for attrib-idx fixnum = (+ entry-attrib i)
          :for this-offset fixnum = (+ offset (* this-comp-count 4 i))
          :for is-int = (eq comp-type :int)
          :for is-double = (eq comp-type :double)
          :when is-int :do
            (gl:vertex-attrib-ipointer attrib-idx
                                       this-comp-count
                                       comp-type
                                       stride
                                       this-offset)
          :when is-double :do
            (%gl:vertex-attrib-lpointer attrib-idx
                                        this-comp-count
                                        comp-type
                                        stride
                                        this-offset)
          :when (and (not is-int) (not is-double)) :do
            (gl:vertex-attrib-pointer attrib-idx
                                      this-comp-count
                                      comp-type
                                      nil
                                      stride
                                      this-offset)
          :do
             (gl:enable-vertex-attrib-array attrib-idx)
             (%gl:vertex-attrib-divisor attrib-idx divisor))))
  t)

(defmethod update ((object buffer) elapsed-seconds)
  (declare (ignorable object elapsed-seconds))
  nil)

(defmethod reload ((buffer buffer))
  (bind buffer)
  (with-slots (pointer free target) buffer
    (when (null pointer)
      (error "Cannot refill buffer from nil."))
    (gl:buffer-sub-data target pointer)
    (when (and free pointer)
      (gl:free-gl-array pointer)
      (setf pointer nil))))



(defun gl-iset (array idx value)
  "Set array position idx to value. value must be a fixnum"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type fixnum value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-fset (array idx value)
  "Set array position idx to value. value is coerced to a single-float."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type single-float value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-dset (array idx value)
  "Set array position idx to value. value is coerced to a double-float."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type double-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-get (array idx)
  "Return the array value at idx."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0))
           (type fixnum idx)
           (type gl:gl-array array))
  (gl:glaref array idx))


(defun to-gl-array (gl-type size data)
  "Create an OpenGL array of the specified type and size, initialized with the contents of arr.  If data is"
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let* ((gl-array (gl:alloc-gl-array gl-type size)))
    (fill-pointer-offset data gl-array 0)
    gl-array))


(defun fill-buffer (buffer data)
  "Fill buffer with data."
  (with-slots (pointer) buffer
    (fill-pointer-offset data pointer 0)))


(defmethod fill-pointer-offset ((data vec2) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (+ 2 offset))


(defmethod fill-pointer-offset ((data vec3) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (+ 3 offset))

(defmethod fill-pointer-offset ((data vec4) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (gl-fset ptr (+ 2 offset) (vz data))
  (gl-fset ptr (+ 3 offset) (vw data))
  (+ 4 offset))

(defmethod fill-pointer-offset ((data mat3) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for off fixnum :from 0
    :for d :of-type real :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))

(defmethod fill-pointer-offset ((data mat4) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for off fixnum :from 0
    :for d :of-type real :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))


(defmethod fill-pointer-offset ((data integer) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (gl-iset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data real) ptr offset)
  (gl-fset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data vector) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for obj :across data
    :for off fixnum = offset :then next-off
    :for next-off fixnum = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defmethod fill-pointer-offset ((data list) ptr offset)
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for obj :in data
    :for off fixnum = offset :then next-off
    :for next-off fixnum = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defun show-gl-array (array &optional count stride)
  "Print the contents of array to standard out."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :for i fixnum :below (if count
                             (min count
                                  (gl::gl-array-size array))
                             (gl::gl-array-size array))
    :do
       (when (and stride (zerop (mod i stride)))
         (terpri))
       (format t "~a " (gl:glaref array i)))
  (terpri))

(defun from-gl-array (array &optional count)
  "Return a Lisp array with the contents of array to standard out."
  (declare (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (loop
    :with real-count fixnum = (if count
                                  (min count
                                       (gl::gl-array-size array))
                                  (gl::gl-array-size array))
    :with rval = (make-array real-count)
    :for i fixnum :below real-count
    :do
       (setf (aref rval i) (gl:glaref array i))
    :finally (return rval)))
