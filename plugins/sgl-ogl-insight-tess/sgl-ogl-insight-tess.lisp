;; sgl-ogl-insight-tess.lisp
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

(in-package :sgl-ogl-insight-tess)

;; (setf sgl:*shader-dirs*
;;       (adjoin (asdf:system-relative-pathname :sgl-ogl-insight-tess "shaders/") sgl:*shader-dirs*))



(defclass ogl-insight-tess (sgl:opengl-object)
  ((sgl:primitive-type :initform :patches)
   (height-map-png-filename :initarg :height-map-png-filename
                            :initform "~/images/quadtrees/test.png"
                            :type (or pathname string))
   (x-steps :type fixnum
            :initform 32
            :initarg :x-steps)
   (y-steps :type fixnum
            :initform 32
            :initarg :y-steps)
   (sgl:styles :initform (list
                          (cons :tess-style (sgl:style-from-directory "~/oss_src/OpenGLInsightsCode/Chapter 10 GPU Tessellation We Still Have a LOD of Terrain to Cover/OpenGLInsightsTessellation/shaders/"
                                                                      "terrain*.*"))))))

(defmethod sgl:initialize-buffers ((object ogl-insight-tess) &key)
  (when (sgl:buffers object)
    (error "Object buffers already setup!"))
  (with-slots (x-steps y-steps height-map-png-filename)  object
    (sgl:set-buffer object
                    :vertices
                    (sgl:constant-attribute-buffer
                     (loop :with x-min = (- (* 2 pi))
                           :with x-max = (* 2 pi)
                           :with y-min = (- (* 2 pi))
                           :with y-max = (* 2 pi)
                           :with dx = (/ (- x-max x-min) x-steps)
                           :with dy = (/ (- y-max y-min) y-steps)

                           :for i :below x-steps
                           :nconcing
                           (loop :for j :below y-steps
                                 :nconcing
                                 (list
                                  (coerce  (+ x-min (* i dx)) 'single-float)
                                  (coerce  (+ y-min (* j dy)) 'single-float)
                                  0.0
                                  (coerce  (+ x-min (* (1+ i) dx)) 'single-float)
                                  (coerce  (+ y-min (* j dy)) 'single-float)
                                  0.0
                                  (coerce  (+ x-min (* (1+ i) dx)) 'single-float)
                                  (coerce  (+ y-min (* (1+ j) dy)) 'single-float)
                                  0.0
                                  (coerce  (+ x-min (* i dx)) 'single-float)
                                  (coerce  (+ y-min (* (1+ j) dy)) 'single-float)
                                  0.0)))
                     (* x-steps y-steps 4 3)
                     '(("in_position" . :vec3))
                     :free nil))
    (sgl:set-buffer object
                  :indices
                  (sgl:constant-index-buffer (* x-steps y-steps 4)
                                             :free nil))
    (sgl:add-texture object (make-instance 'sgl:png-texture :filename height-map-png-filename))))

(defmethod sgl:render ((object ogl-insight-tess))
  (with-slots (sgl:buffers sgl:uniforms sgl:primitive-type sgl:styles
               x-steps y-steps) object
    (sgl:bind object)
    (loop :for (nil . style) :in sgl:styles :do
      (sgl:use-style style)
      (loop :for (nil . uniform) :in sgl:uniforms :do
        (sgl:use-uniform uniform (sgl:program style)))
      (gl:patch-parameter :patch-vertices 4)
      (gl:draw-elements sgl:primitive-type
                        (gl:make-null-gl-array :unsigned-int)
                        :count (sgl:idx-count (assoc-value sgl:buffers :indices))))))
