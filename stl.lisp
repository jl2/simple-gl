;; stl.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass stl-file (instanced-opengl-object)
  ((file-name :initarg :file-name :type (or string path))
   (tri-count :initform 0 :type (unsigned-byte 32))))

(declaim (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum) (unsigned-byte 16)) get-u2)
         (inline get-u2))
(defun get-u2 (arr idx)
  "Interpret two bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 16) (+ (* (aref arr (1+ idx)) 256) (aref arr idx))))

(declaim (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum) (unsigned-byte 32)) get-u4)
         (inline get-u4))
(defun get-u4 (arr idx)
  "Interpret the four bytes in arr as an '(unsigned-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (unsigned-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 3 idx)) 256)
                                         (aref arr (+ 2 idx))) 256)
                                   (aref arr (+ 1 idx))) 256)
                             (aref arr idx))))

(declaim (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum) (signed-byte 32)) get-s4)
         (inline get-s4))
(defun get-s4 (arr idx)
  "Interpret four bytes in arr as an '(signed-byte 32)"
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (the (signed-byte 32) (+ (* (+ (* (+ (* (aref arr (+ 3 idx)) 256)
                                       (aref arr (+ 2 idx))) 256)
                                 (aref arr (+ 1 idx))) 256)
                           (aref arr idx))))
(declaim (ftype (function
                 ((simple-array (unsigned-byte 8)) fixnum) single-float) get-float)
         (inline get-float))
(defun get-float (arr idx)
  "Interpret four bytes in arr as a single-float."
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr))
  (let ((x (get-u4 arr idx)))
    #+(and :little-endian :ieee-floating-point :sbcl)
    (if (>= x #x80000000)
        (sb-kernel:make-single-float (- x #x100000000))
        (sb-kernel:make-single-float x))
    #-(and :little-endian :ieee-floating-point :sbcl)
    (ieee-floats:decode-float32 x)))

(defparameter *float-byte-size* 4
  "Size of an STL float in bytes.")

(defparameter *point-byte-size* (* 3 *float-byte-size*)
  "Size of an STL point in bytes.")

(defparameter *triangle-byte-size* (+ 2 (* 4 *point-byte-size*))
  "Size of an STL triangle in bytes.")
(defun get-point (arr idx)
  "Create a point using x, y, and z values read from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8) *) arr))
  (vec3 (get-float arr idx)
        (get-float arr (+ idx *float-byte-size*))
        (get-float arr (+ idx (* 2 *float-byte-size*)))))


(declaim (type (unsigned-byte 32) *point-byte-size*))
(defun read-triangle (arr idx)
  "Read a triangle from arr."
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0))
   (type (simple-array (unsigned-byte 8)) arr)
   (type (unsigned-byte 32) idx)
   (type (unsigned-byte 32)  *point-byte-size*))
  (values
   ;; Point 0
   (get-point arr idx)
   ;; Point 1
   (get-point arr (+ (* 1 *point-byte-size*) idx))
   ;; Point 2
   (get-point arr (+ (* 2 *point-byte-size*) idx))
   ;; Color
   (get-point arr (+ (* 3 *point-byte-size*) idx))
   ;; Tag
   (coerce (get-u2 arr (+ (* 4 *point-byte-size*) idx)) 'single-float)))

(defun get-stl-info (stl-obj)
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (with-slots (file-name tri-count) stl-obj
    (with-open-file (inf file-name :element-type '(unsigned-byte 8))
      (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
            (triangle-count-buffer (make-array 4 :element-type '(unsigned-byte 8))))
        (read-sequence header inf)
        (read-sequence triangle-count-buffer inf)
        (format t "STL File has ~a triangles!~%" tri-count)
        (values (get-u4 triangle-count-buffer 0)
                header
                (babel:octets-to-string header :end (position 0 header)))))))


(defmethod initialize-buffers ((obj stl-file) &key)
  (declare
   (optimize (speed 3) (space 0) (safety 0) (debug 0)))
  (with-slots (file-name tri-count) obj
    (let ((vertices )
          (buffer (make-array *triangle-byte-size* :element-type '(unsigned-byte 8)))
          (cur-offset 0))
      (with-open-file (inf file-name :element-type '(unsigned-byte 8))
        (let ((header (make-array 80 :element-type '(unsigned-byte 8)))
              (triangle-count-buffer (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence header inf)
          (read-sequence triangle-count-buffer inf)
          (setf tri-count (get-u4 triangle-count-buffer 0))
          (format t "STL File has ~a triangles!~%" tri-count)
          (setf vertices (gl:alloc-gl-array :float( * tri-count (* 3 6))))
          (loop
            :for idx fixnum :below tri-count
            :for last-idx fixnum = (read-sequence buffer inf)
            :while (= last-idx  *triangle-byte-size*)
            :do
               (multiple-value-bind (norm p1 p2 p3) (read-triangle buffer 0)

                 (when (v= (vec3 0 0 0) norm)
;;                   (format t "Found 0 normal! ~a ~a ~a~%" p1 p2 p3)
                   (setf norm (vunit (vc (v- p1 p2) (v- p1 p3) ))))


                 (setf cur-offset (fill-pointer-offset p1 vertices cur-offset))

                 (setf cur-offset (fill-pointer-offset norm vertices cur-offset))

                 (setf cur-offset (fill-pointer-offset p2 vertices cur-offset))

                 (setf cur-offset (fill-pointer-offset norm vertices cur-offset))

                 (setf cur-offset (fill-pointer-offset p3 vertices cur-offset))

                 (setf cur-offset (fill-pointer-offset norm vertices cur-offset))))))
      (set-buffer obj :vertices (make-instance 'attribute-buffer
                                               :pointer vertices
                                               :attributes '(("in_position" . :vec3)
                                                             ("in_normal" . :vec3))
                                               :free t))
      (set-buffer obj :indices (constant-index-buffer (* 3 tri-count)))
      (with-slots (instance-count) obj
        (setf instance-count 1)
        (set-buffer obj :colors (make-instance 'instance-buffer
                                               :pointer (to-gl-array :float (* 4 instance-count) (vec4 0.1 0.8 0.2 1.0))
                                               :attributes '(("in_color" . :vec4))
                                               :free t))
        (set-buffer obj :transforms (make-instance 'instance-buffer
                                                   :pointer (to-gl-array :float (* 16 instance-count) (meye 4))
                                                   :free t))))))
