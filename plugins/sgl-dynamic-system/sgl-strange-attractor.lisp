;; sgl-strange-attractor.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :sgl-dynamic-system)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-dynamic-system "shaders/") sgl:*shader-dirs*))


(defclass sgl-strange-attractor (opengl-object)
  ((primitive-type :initform :points)
   (point-count :initform 10000 :initarg :point-count)
   (radius :initarg :radius :initform 1.0)
   (a :initarg :a :initform 2.24)
   (b :initarg :b :initform 0.43)
   (c :initarg :c :initform -0.65)
   (d :initarg :d :initform -2.43)
   (e :initarg :e :initform 1.0)))

(defmethod initialize-uniforms ((object sgl-strange-attractor) &key)
  (with-slots (a b c d e) object
    (set-uniform object "a" a :float)
    (set-uniform object "b" b :float)
    (set-uniform object "c" c :float)
    (set-uniform object "d" d :float)
    (set-uniform object "e" e :float)))

(defmethod initialize-buffers ((object sgl-strange-attractor) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (point-count radius) object
    (let ((data (loop
                  for i below point-count
                  for pt = (vec3-random (- radius) radius)
                  collecting pt))
           (indices (loop for i below point-count collecting i)))
      (set-buffer object
                :vertices
                (make-instance
                 'attribute-buffer
                 :pointer (to-gl-array :float (* 7 point-count) data)
                 :stride 7
                 :attributes '(("in_position" . :vec3))
                 :usage :static-draw
                 :free t))
      (set-buffer object
                  :indices
                  (make-instance
                   'index-buffer
                   :idx-count point-count
                   :pointer (to-gl-array :unsigned-int (length indices) indices)
                   :stride 1
                   :usage :static-draw
                   :free t))
      (set-buffer object
                  :obj-transform
                  (make-instance
                   'instance-buffer
                   :pointer (to-gl-array :float 16 (meye 4))
                   :stride 16
                   :attributes '(("obj_transform" . :mat4))
                   :usage :static-draw
                   :free t))
      (set-buffer object
                  :color
                  (make-instance
                   'instance-buffer
                   :pointer (to-gl-array :float 4 (vec4 0.5 0.5 0.5 0.125))
                   :stride 4
                   :attributes '(("in_color" . :vec4))
                   :usage :static-draw
                   :free t)))))

(defun create-strange-attractor (point-count radius
                                 &key
                                   (a (ju:random-between -2.0 2.0))
                                   (b (ju:random-between -2.0 2.0))
                                   (c (ju:random-between -2.0 2.0))
                                   (d (ju:random-between -2.0 2.0))
                                   (e (ju:random-between -2.0 2.0)))
  (let (
        (obj (make-instance
              'sgl-strange-attractor
              :point-count point-count
              :radius radius
              :a a
              :b b
              :c c
              :d d
              :e e
              :styles (list (cons :strange-attractor
                                  (make-instance 'style
                                                 :shaders (list (sgl:read-shader "sa-vertex.glsl")
                                                                (sgl:read-shader "sa-fragment.glsl")
                                                                (sgl:read-shader "sa-geometry.glsl"))))))))
    obj))
