;; buffer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defmethod bind ((buffer buffer))
  (with-slots (bo target usage free pointer) buffer
    (cond ((= bo 0)
           (when (null pointer)
             (error "Cannot fill buffer from nil."))
           (setf bo (car (gl:gen-buffers 1)))
           (gl:bind-buffer target bo)
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
  ;; (format t "buffer ~a program ~a~%" buffer program)
  (with-slots (attributes divisor) buffer
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
                  ;;                           (+ offset (* comp-count 4 i)) ~a~%"
                  ;;         attrib-idx
                  ;;         this-comp-count
                  ;;         comp-type
                  ;;         :false
                  ;;         stride
                  ;;         this-offset)

                  (%gl:vertex-attrib-divisor attrib-idx divisor))))))
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


(declaim (inline
          gl-fset gl-iset gl-dset
          gl-get
          to-gl-array
          fill-pointer-offset
          fill-buffer))
(defun gl-iset (array idx value)
  "Set array position idx to value. value must be a fixnum"
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type fixnum value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-fset (array idx value)
  "Set array position idx to value. value is coerced to a single-float."
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type single-float value)
           (type gl:gl-array array))
  (when (> idx (gl::gl-array-size array))
    (error "~a is outside of bounds for array." idx))
  (setf (gl:glaref array idx) value))

(defun gl-dset (array idx value)
  "Set array position idx to value. value is coerced to a double-float."
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type double-float value)
           (type gl:gl-array array))
  (setf (gl:glaref array idx) value))

(defun gl-get (array idx)
  "Return the array value at idx."
  (declare (optimize (speed 3))
           (type fixnum idx)
           (type gl:gl-array array))
  (gl:glaref array idx))


(defun to-gl-array (gl-type size data)
  "Create an OpenGL array of the specified type and size, initialized with the contents of arr.  If data is"
  (declare (optimize (speed 3)))
  (let* ((gl-array (gl:alloc-gl-array gl-type size)))
    (fill-pointer-offset data gl-array 0)
    gl-array))


(defun fill-buffer (buffer data)
  "Fill buffer with data."
  (with-slots (pointer) buffer
    (fill-pointer-offset data pointer 0)))


(defmethod fill-pointer-offset ((data vec2) ptr offset)
  (gl-fset ptr (+ 0 offset) (vx data))
  (gl-fset ptr (+ 1 offset) (vy data))
  (+ 2 offset))


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
  (loop
    :for off :from 0
    :for d :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))

(defmethod fill-pointer-offset ((data mat4) ptr offset)
  (loop
    :for off :from 0
    :for d :across (marr (mtranspose data))
    :do
       (gl-fset ptr (+ off offset) d)
    :finally (return (+ off offset))))


(defmethod fill-pointer-offset ((data integer) ptr offset)
  (gl-iset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data real) ptr offset)
  (gl-fset ptr offset data)
  (1+ offset))

(defmethod fill-pointer-offset ((data vector) ptr offset)
  (loop
    :for obj :across data
    :for off = offset :then next-off
    :for next-off = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defmethod fill-pointer-offset ((data list) ptr offset)
  (loop
    :for obj :in data
    :for off = offset :then next-off
    :for next-off = (fill-pointer-offset obj ptr off)
    :finally (return next-off)))

(defun show-gl-array (array &optional count)
  "Print the contents of array to standard out."
  (loop
    :for i :below (if count
                      (min count
                           (gl::gl-array-size array))
                      (gl::gl-array-size array))
    :do
       (format t "~a " (gl:glaref array i)))
  (terpri))
