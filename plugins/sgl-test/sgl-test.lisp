;; sgl-test.lisp
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

(in-package :sgl-test)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-test "shaders/") sgl:*shader-dirs*))

(defun tess-test ()
  (make-instance 'tesselation-shader-test :inner 2 :outer 4))

(defclass tesselation-shader-test (sgl:instanced-opengl-object)
  ((primitive-type :initform :patches)
   (inner :initform 2 :initarg :inner)
   (outer :initform 4 :initarg :outer)
   (style :initform (make-style-from-files "tess-test"
                                           "tess-test-vertex.glsl"
                                           "tess-test-tess-control.glsl"
                                           "tess-test-tess-eval.glsl"
                                           "tess-test-geometry.glsl"
                                           "tess-test-fragment.glsl")))
  )

(defmethod sgl:render ((object tesselation-shader-test))
  (with-slots (buffers uniforms primitive-type style instance-count
               inner outer) object
    (bind object)
    (use-style style)

    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))

    (when (> instance-count 0)
      (gl:patch-parameter :patch-vertices 3)

      (set-uniform object "color" (vec4 0.2f0 0.9f0 0.2f0 1.0f0) :vec4)
      (gl:polygon-mode :front-and-back :fill)
      (gl:draw-elements-instanced primitive-type
                                  (gl:make-null-gl-array :unsigned-int)
                                  instance-count
                                  :count (idx-count (assoc-value buffers :indices)))

      (gl:polygon-mode :front-and-back :line)
      (set-uniform object "color" (vec4 1.0f0 0.9f0 1.0f0 1.0f0) :vec4)
      (gl:draw-elements-instanced primitive-type
                                  (gl:make-null-gl-array :unsigned-int)
                                  instance-count
                                  :count (idx-count (assoc-value buffers :indices)))
      ))
  nil)

(defmethod sgl:initialize-buffers ((object tesselation-shader-test) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (set-buffer object
              :vertices
              (make-instance
               'attribute-buffer
               :pointer (to-gl-array
                         :float
                         4
                         (list -1.0f0 0.0f0 -1.0f0
                               -1.0f0 0.0f0  1.0f0
                               1.0f0 0.0f0  1.0f0
                               1.0f0 0.0f0 -1.0f0))
               :stride 3
               :attributes '(("in_position" . :vec2))
               :usage :static-draw
               :free t))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2
                                                       2 3 1))
               :stride nil
               :usage :static-draw
               :free t))
  (set-buffer object
              :obj-transform (make-instance
                          'instance-buffer
                          :pointer (to-gl-array :float 16 (list
                                                           (meye 4)))
                          :stride nil
                          :attributes '(("obj_transform" . :mat4))
                          :usage :static-draw
                          :free nil))
  (setf (slot-value object 'instance-count) 1))


(defmethod sgl:initialize-uniforms ((object tesselation-shader-test) &key)
  (with-slots (in-size inner outer) object
    (set-uniform object "inner" inner :float)
    (set-uniform object "outer" outer :float))
  t)
