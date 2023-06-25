;; gl-array-utils.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

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

(defun from-gl-array (array &optional count)
  "Return a Lisp array with the contents of array to standard out."
  (let* ((real-count (if count
                      (min count
                           (gl::gl-array-size array))
                      (gl::gl-array-size array)))
         (rval (make-array real-count)))
    (loop
      :for i :below real-count
      :do
         (setf (aref rval i) (gl:glaref array i)))
    rval))
