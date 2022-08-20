;; sgl-blend2d-texture.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :sgl-blend2d-texture)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-blend2d-texture "shaders/") sgl:*shader-dirs*))

(defclass sgl-blend2d-texture (texture)
  ((tex-type :initform :texture-2d)
   (size :initarg :size :initform 2048)
   (textures :initform nil))
  (:documentation "Texture drawn by Blend2D."))

(defgeneric draw-image (obj img ctx size))

(defmethod draw-image ((obj sgl-blend2d-texture) img ctx size)
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

(defmethod fill-texture ((obj sgl-blend2d-texture) elapsed-time)
  (with-slots (size textures) obj
    (bl:with-memory-image-context*
        (img ctx :width size :height size)
        ((data bl:image-data))
      (draw-image obj img ctx size)
      (bl:image-get-data img data)
      (gl:tex-image-2d :texture-2d 0 :rgba size size 0 :rgba :unsigned-byte (bl:image-data.pixel-data data))
      (gl:generate-mipmap :texture-2d))))
