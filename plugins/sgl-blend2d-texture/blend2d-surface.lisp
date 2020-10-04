;; blend2d-surface.lisp
;;
;; Copyright (c) 2020 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>


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

(in-package :blend2d-surface)

(defparameter *shader-dir* (asdf:system-relative-pathname :blend2d-surface "shaders/")
  "Directory containing newgl shaders.")

(defclass blend2d-surface (newgl:geometry)
  ((newgl:primitive-type :initform :triangles)
   (newgl:shader-program :initform (newgl:make-shader-program
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "bls-vertex.glsl"))
                                    (newgl:shader-from-file (merge-pathnames *shader-dir* "bls-fragment.glsl"))))
   (size :initarg :size :initform 1024)
   (textures :initform nil))
  (:documentation "Base class for all objects that can be rendered in a scene."))


(defmethod newgl:vertex-buffers ((object blend2d-surface))
  (values (make-array
           (* 4 (+ 3 2))
           :element-type 'single-float
           :initial-contents '(-1.0f0 1.0f0 0.0f0  0.0f0 1.0f0
                               -1.0f0  -1.0f0 0.0f0  0.0f0 0.0f0
                               1.0f0  1.0f0 0.0f0  1.0f0 1.0f0
                               1.0f0 -1.0f0 0.0f0  1.0f0 0.0f0))
          (make-array
                       6
                       :element-type 'fixnum
                       :initial-contents '(0 1 2 1 3 2))))

(defgeneric draw-image (obj img ctx size))

(defmethod draw-image ((obj blend2d-surface) img ctx size)
  (declare (ignorable obj img ctx size))
  (bl:with-objects
      ((circle bl:circle))
    (dotimes (i 2000)
        (let* ((sx (random (coerce size 'double-float)))
               (sy (random (coerce size 'double-float)))
               (radius (coerce (random (/ size 50.0)) 'double-float)))

          (setf (bl:circle.cx circle) sx)
          (setf (bl:circle.cy circle) sy)
          (setf (bl:circle.r circle) radius)
          (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
          (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff)))
          (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))))))

(defgeneric draw-texture (obj))

(defmethod draw-texture ((obj blend2d-surface))
  (with-slots (size textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d :texture-2d 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap :texture-2d))))


(defmethod newgl:fill-buffers ((object blend2d-surface))
  (call-next-method)
  (with-slots (textures size) object
    (when textures
      (error "fill-buffers called twice!"))
    (setf textures (gl:gen-textures 1))
    (gl:bind-texture :texture-2d (car textures))
    (gl:tex-parameter :texture-2d :texture-wrap-s :repeat)
    (gl:tex-parameter :texture-2d :texture-wrap-t :repeat)
    (gl:tex-parameter :texture-2d :texture-base-level 0)
    (gl:tex-parameter :texture-2d :texture-max-level 8)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (draw-texture object)))

(defmethod newgl:bind-buffers ((object blend2d-surface))
  (call-next-method)
  (with-slots (textures) object
    (gl:bind-texture :texture-2d (car textures))))

(defmethod newgl:cleanup ((object blend2d-surface))
  (with-slots (textures) object
    (when textures
      (gl:bind-texture :texture-2d 0)
      (gl:delete-textures textures)
      (setf textures nil)))
  (call-next-method))
