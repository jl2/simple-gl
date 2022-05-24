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

(defun tess-test ( &key (inner 7) (outer 7))
  (make-instance 'tesselation-shader-test :inner inner :outer outer))

(defclass tesselation-shader-test (sgl:opengl-object)
  ((primitive-type :initform :patches)
   (inner :initform 8 :initarg :inner)
   (outer :initform 8 :initarg :outer)
   (style :initform (make-style-from-files "tess-test"
                                           "tess-test-vertex.glsl"
                                           "tess-test-tess-control.glsl"
                                           "tess-test-tess-eval.glsl"
                                           "tess-test-geometry.glsl"
                                           "tess-test-fragment.glsl"))))

(defmethod sgl:render ((object tesselation-shader-test))
  (with-slots (buffers uniforms primitive-type style
               inner outer) object
    (bind object)

    (set-uniform object "in_color" (vec4 0.2f0 0.9f0 0.2f0 1.0f0) :vec4)
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))

    (use-style style)


    (gl:patch-parameter :patch-vertices 16)

    (gl:polygon-mode :front-and-back :fill)
    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count (idx-count (assoc-value buffers :indices)))

    (set-uniform object "in_color" (vec4 0.0f0 0.0f0 0.0f0 1.0f0) :vec4)
    (dolist (uniform uniforms)
      (use-uniform (cdr uniform) (program style)))

    (gl:polygon-mode :front-and-back :line)
    (gl:draw-elements primitive-type
                      (gl:make-null-gl-array :unsigned-int)
                      :count (idx-count (assoc-value buffers :indices)))
    )
  nil)

(defmethod sgl:initialize-buffers ((object tesselation-shader-test) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (let* ((two-third (coerce (/ 2 3) 'single-float))
        (neg-two-third (- two-third)))
    (set-buffer object
                :vertices
                (make-instance
                 'attribute-buffer
                 :pointer (to-gl-array
                           :float
                           (* 16 3)
                           (list -1.0f0 -1.0f0 0.0f0
                                 -1.0f0 neg-two-third 0.f0
                                 -1.0f0 two-third 0.0f0
                                 -1.0f0 1.0f0 0.0f0

                                 neg-two-third -1.0f0 0.0f0
                                 neg-two-third neg-two-third -2.0f0
                                 neg-two-third two-third 0.0f0
                                 neg-two-third 1.0f0 0.0f0

                                 two-third -1.0f0 0.0f0
                                 two-third neg-two-third 0.0f0
                                 two-third two-third 2.0f0
                                 two-third 1.0f0 0.0f0

                                 1.0f0 -1.0f0 0.0f0
                                 1.0f0 neg-two-third 0.0f0
                                 1.0f0 two-third 0.0f0
                                 1.0f0 1.0f0 0.0f0
                                 ))
                 :stride nil
                 :attributes '(("in_position" . :vec3))
                 :usage :static-draw
                 :free t)))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 16
               :pointer (to-gl-array :unsigned-int 16 (loop for i below 16 collecting i))
               :stride nil
               :usage :static-draw
               :free t))
  )


(defmethod sgl:initialize-uniforms ((object tesselation-shader-test) &key)
  (with-slots (in-size inner outer) object
    (set-uniform object "inner" inner :float)
    (set-uniform object "outer" outer :float)
    (set-uniform object "obj_transform" (meye 4) :mat4)
    (set-uniform object "view_transform" (meye 4) :mat4))
  t)
