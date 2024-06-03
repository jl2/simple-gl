;; sgl-fractal-landscape.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :sgl-fractal-landscape)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-fractal-landscape "shaders/") sgl:*shader-dirs*))



(defclass fractal-landscape (sgl:opengl-object)
  ((sgl:primitive-type :initform :patches)
   (sgl:styles :initform (list
                          `(:obj-style
                            .
                            ,(sgl:make-style-from-files "landscape-vertex.glsl"
                                                        "landscape-plastic-fragment.glsl"
                                                        "landscape-tess-eval.glsl"
                                                        "landscape-tess-control.glsl"
                                                        "landscape-geometry.glsl"
                                                        )))
               :initarg :styles)))

(defmethod sgl:initialize-buffers ((object fractal-landscape) &key)
  (when (sgl:buffers object)
    (error "Object buffers already setup!"))
  (sgl:set-buffer object
                  :vertices
                  (sgl:constant-attribute-buffer
                   '( -1.0f0 -1.0f0 0.0f0
                     -1.0f0 1.0f0 0.0f0
                     1.0f0 1.0f0 0.0f0
                     1.0f0 -1.0f0 0.0f0
                     )
                   (* 4 3)
                   '(("in_position" . :vec3))
                   :free nil))

  (sgl:set-buffer object
                  :indices
                  (sgl:constant-index-buffer 4
                                             :free nil)))

(defmethod sgl:render ((object fractal-landscape))
  (with-slots (sgl:buffers sgl:uniforms sgl:primitive-type sgl:styles
               inner outer) object
    (sgl:bind object)
    (loop :for (nil . style) :in sgl:styles :do
      (sgl:use-style style)
      (loop :for (nil . uniform) :in sgl:uniforms :do
        (sgl:use-uniform uniform (sgl:program style)))
      (gl:patch-parameter :patch-vertices 4)
      (gl:polygon-mode :front-and-back :line)
      (gl:draw-elements sgl:primitive-type
                        (gl:make-null-gl-array :unsigned-int)
                        :count (sgl:idx-count (assoc-value sgl:buffers :indices))))))
