;; buffer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)


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
   (attributes :initform '(("in_position" . :vec3)
                           ("in_color" . :vec4))
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
  ((max-instances)
   (attributes :initform '(("obj_transform" . :mat4)) :initarg :attributes)
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

(defun constant-index-buffer (count &key (free t))
  (make-instance 'index-buffer
                 :idx-count count
                 :pointer (to-gl-array :unsigned-int
                                       count
                                       (loop :for i :below count :collecting i))
                 :free free))

(defun constant-instance-buffer (data float-count attributes &key (free t) (usage :static-draw))
  (make-instance 'attribute-buffer
                 :pointer (to-gl-array :float
                                       float-count
                                       data)
                 :stride nil
                 :attributes attributes
                 :usage usage
                 :free free))

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
  ;; (format t "buffer ~a program ~a~%" buffer program)
  (with-slots (attributes) buffer
    (loop
      :with stride = (compute-stride buffer)
      :for offset = 0 :then (+ offset byte-size)
      :for (name . type) :in attributes
      :for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
      :for this-comp-count = (min comp-count 4)
      :do
         (let ((entry-attrib (gl:get-attrib-location program name)))
           ;; (format t "entry-attrib: ~a program ~a name ~a~%" entry-attrib program name)
           (when (>= entry-attrib 0)
             (loop
               :for i :below vec4-size
               :for attrib-idx = (+ entry-attrib i)
               :for this-offset = (+ offset (* this-comp-count 4 i))
               :do
                  (cond ((eq comp-type :int)
                          (gl:vertex-attrib-ipointer attrib-idx
                                                     this-comp-count
                                                     comp-type
                                                     stride
                                                     this-offset))
                        ;; ((eq comp-type :double)
                        ;;   (gl:vertex-attrib-lpointer attrib-idx
                        ;;                              this-comp-count
                        ;;                              comp-type
                        ;;                              :false
                        ;;                              stride
                        ;;                              this-offset))
                        (t
                         (gl:vertex-attrib-pointer attrib-idx
                                                   this-comp-count
                                                   comp-type
                                                   :false
                                                   stride
                                                   this-offset)))
                  (gl:enable-vertex-attrib-array attrib-idx)
                  ;; (format t "attrib-idx ~a~%~
                  ;;                           comp-count ~a~%~
                  ;;                           comp-type ~a~%~
                  ;;                           :false ~a~%~
                  ;;                           stride ~a~%~
                  ;;                           this-offset ~a~%"
                  ;;         attrib-idx
                  ;;         this-comp-count
                  ;;         comp-type
                  ;;         :false
                  ;;         stride
                  ;;         this-offset)
               )))))
  t)

(defmethod associate-attributes ((buffer instance-buffer) program)
  (with-slots (attributes divisor) buffer
    (loop
      :with stride = (compute-stride buffer)
      :for offset = 0 :then (+ offset byte-size)
      :for (name . type) :in attributes
      :for (comp-type comp-count byte-size vec4-size) = (glsl-type-info type)
      :for this-comp-count = (min comp-count 4)
      :do
         (let ((entry-attrib (gl:get-attrib-location program name)))
           (when (>= entry-attrib 0)
             (loop
               :for i :below vec4-size
               :for attrib-idx = (+ entry-attrib i)
               :for this-offset = (+ offset (* this-comp-count 4 i))
               :do
                  (cond ((eq comp-type :int)
                         (gl:vertex-attrib-ipointer attrib-idx
                                                    this-comp-count
                                                    comp-type
                                                    stride
                                                    this-offset))
                        ;; ((eq comp-type :double)
                        ;;   (gl:vertex-attrib-lpointer attrib-idx
                        ;;                              this-comp-count
                        ;;                              comp-type
                        ;;                              :false
                        ;;                              stride
                        ;;                              this-offset))
                        (t
                         (gl:vertex-attrib-pointer attrib-idx
                                                   this-comp-count
                                                   comp-type
                                                   :false
                                                   stride
                                                   this-offset)))
                  (gl:enable-vertex-attrib-array attrib-idx)
                  ;; (format t "attrib-idx ~a~%~
                  ;;                           comp-count ~a~%~
                  ;;                           comp-type ~a~%~
                  ;;                           :false ~a~%~
                  ;;                           stride ~a~%~
                  ;;                           (+ offset (* comp-count 4 i)) ~a~%"
                  ;;         attrib-idx
                  ;;         this-comp-count
                  ;;         comp-type
                  ;;         :false
                  ;;         stride
                  ;;         this-offset)
                  (%gl:vertex-attrib-divisor attrib-idx divisor))))))
  t)

(defmethod initialize ((buffer attribute-buffer) &key)
  (with-slots (bo) buffer
    (when (= bo 0)
      (setf bo (car (gl:gen-buffers 1))))))


(defmethod bind ((buffer buffer))
  (with-slots (bo target usage) buffer
    (when (= bo 0)
      (error "Cannot bind uninitialized buffer."))
    (gl:bind-buffer target bo)))


(defmethod update ((object buffer) elapsed-seconds)
  (declare (ignorable object elapsed-seconds))
  nil)

(defmethod load-gl ((buffer attribute-buffer))
  (with-slots (pointer usage free target) buffer
    (when (not (null pointer))
      (error "Cannot load-gl a previously loaded buffer."))
    (setf pointer (to-gl-array :float
                               (* (+ 3 4) 3)
                               (list
                                (vec3 -1.0f0 -1.0f0 0.0f0)
                                (vec4 0.1f0 0.8f0 0.1f0 1.0f0)
                                (vec3 1.0f0 -1.0f0 0.0f0)
                                (vec4 0.1f0 0.8f0 0.1f0 1.0f0)
                                (vec3 0.0f0 1.0f0 0.0f0)
                                (vec4 0.1f0 0.8f0 0.1f0 1.0f0))))
    (gl:buffer-data target usage pointer)
    (when (and free pointer)
      (gl:free-gl-array pointer)
      (setf pointer nil))))

(defmethod reload-gl ((buffer buffer))
  (bind buffer)
  (with-slots (pointer free target) buffer
    (when (null pointer)
      (error "Cannot reload buffer from nil."))
    (gl:buffer-sub-data target pointer)
    (when (and free pointer)
      (gl:free-gl-array pointer)
      (setf pointer nil))))

(defun fill-buffer (buffer data)
  "Fill buffer with data."
  (with-slots (pointer) buffer
    (fill-pointer-offset data pointer 0)))
