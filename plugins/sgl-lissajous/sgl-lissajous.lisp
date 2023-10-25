;; sgl-lissajous.lisp

;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :sgl-lissajous)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-lissajous "shaders/") sgl:*shader-dirs*))

(defun lissajous (r tv theta phi)
  (vec3 (* 
           r
           (sin (* r tv theta))
           (cos (* tv phi)))
        (* 
           r
           (sin (* r (sin (* tv phi)) tv theta))
           (sin (* tv phi)))
        (* 
           r
           (cos (* r tv phi theta)))
        ))

(defclass sgl-lissajous (sgl:instanced-opengl-object)
  ((sgl:primitive-type :initform :points :initarg :primitive-type)
   (iterations :initform 10000 :initarg :iterations)
   (theta :initform (sgl:deg2rad 30) :initarg :theta)
   (phi :initform (sgl:deg2rad 12) :initarg :phi)
   (rep :initform 20.0f0 :initarg :rep)
   (r :initform 20.0f0 :initarg :r)
   (r2 :initform 2.0f0 :initarg :r2)))

(defmethod sgl:initialize-buffers ((object sgl-lissajous) &key)
  (when (sgl:buffers object)
    (error "Object buffers already setup!"))
  (with-slots (theta phi rep r r2 iterations sgl:instance-count) object
    (let* (
           (data (loop
                   :with dt = (/ (* rep pi)
                                 iterations)
                   :for i :below iterations
                   :for tv = (* i dt)
                   :collecting (v+ (lissajous r2 theta phi (* 1.787923 (+ dt tv)))
                                   (lissajous r phi theta (* 1.787923 (+ dt tv)))
                                   (lissajous r theta phi (* 2 (+ dt tv))))
                   :collecting (let* ((r (random 1.0))
                                      (g (+ 0.5 (random 0.5)))
                                      (b (random 1.0))
                                      (alpha 0.5))
                                 (vec4 r g b alpha))
                   :collecting (v+ (lissajous r theta phi (* 1.787923 (+ dt tv)))
                                   (lissajous r2 theta phi (* 2 (+ dt tv))))

                   :collecting (let* ((r (random 1.0))
                                      (g (+ 0.5 (random 0.5)))
                                      (b (random 1.0))
                                      (alpha 0.5))
                                 (vec4 r g b alpha))))

           ;; (vertices (loop
           ;;         :with dt = (/ (* rep pi)
           ;;                       iterations)
           ;;         :for i :below iterations
           ;;         :for tv = (* i dt)
           ;;         :collecting (lissajous r theta phi tv)))
           ;; (colors (loop
           ;;         :for i :below iterations
           ;;         :collecting (let* ((r (random 1.0))
           ;;                            (g (+ 0.5 (random 0.5)))
           ;;                            (b (random 1.0))
           ;;                            (alpha 0.5))
           ;;                       (vec4 r g b alpha))))
           )

      (sgl:set-buffer object
                      :vertices
                      (make-instance 'sgl:attribute-buffer
                                     :pointer (sgl:to-gl-array :float
                                                               (* 7 (length data))
                                                               data)
                                     :stride nil
                                     :attributes '(("in_position" . :vec3)
                                                   ("in_color" . :vec4))
                                     :free nil))
      ;; (sgl:set-buffer object
      ;;                 :colors
      ;;                 (make-instance 'sgl:attribute-buffer
      ;;                                :pointer (sgl:to-gl-array :float
      ;;                                                      (* 4 iterations)
      ;;                                                      colors)
                                     
      ;;                                :attributes '(("in_color" . :vec4))
      ;;                                :free t))
      (sgl:set-buffer object
                      :indices (sgl:constant-index-buffer (/ (length data) 2) :free nil))
      (sgl:set-buffer object
                      :obj-transform
                      (make-instance 'sgl:instance-buffer
                                     :pointer (sgl:to-gl-array :float
                                                               (* 1  16)
                                                               (list (meye 4)))
                                     :free nil
                                     :attributes '(("obj_transform" . :mat4))))
      (setf sgl:instance-count 1))))


(defun create-lissajous (&key
                           (iterations 10)
                           (theta (sgl:deg2rad 30))
                           (phi (sgl:deg2rad 12))
                           (rep 20.0f0)
                           (r1 20.0f0)
                           (r2 2.0f0))
  (let ((obj (make-instance 'sgl-lissajous
                            :iterations iterations
                            :theta theta
                            :phi phi
                            :r r1
                            :r2 r2
                            :rep rep
                            :styles (list
                                     (cons
                                      :lissajous
                                      (sgl:make-style-from-files "lissajous-vertex.glsl"
                                                                 "lissajous-fragment.glsl"
                                                                 "lissajous-geometry.glsl"
                                                                 ))))))
    obj))
