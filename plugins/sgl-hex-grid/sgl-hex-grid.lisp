;; sgl-hex-grid.lisp
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

(in-package :sgl-hex-grid)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-hex-grid "shaders/") sgl:*shader-dirs*))

(defclass sgl-hex-grid (opengl-object)
  ((hex-grid :initform (make-instance 'hg:hex-grid) :initarg :hex-grid)
   (primitive-type :initform :points)
   (style :initarg :style
          :initform (make-instance
                     'style :name "hexagons"
                            :shaders (list (sgl:read-shader "hex-vertex.glsl")
                                           (sgl:read-shader "hex-geometry.glsl")
                                           (sgl:read-shader "hex-fragment.glsl"))
                            ;; :shaders (list (sgl:read-shader "dumb-vertex.glsl")
                            ;;                (sgl:read-shader "dumb-geometry.glsl")
                            ;;                (sgl:read-shader "dumb-fragment.glsl"))

                            :poly-mode :fill))))

(defmethod initialize-uniforms ((object sgl-hex-grid) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4))

(defun show-gl-array (ptr count)
  (loop
    :for i :below count
    :do
       (format t "~a " (gl:glaref ptr i)))
  (terpri))

(defmethod update ((object sgl-hex-grid) elapsed-seconds)
  (declare (ignorable elapsed-seconds))
  (with-slots (hex-grid) object
    (with-slots (hg:hex-count) hex-grid
      (let ((buffer (get-buffer object :states)))
        (with-slots (pointer) buffer
          (loop
            :for i :below hg:hex-count
            :do
               (setf (gl:glaref pointer i)
                     (mod (logand i (+ (random 5) (* i i))) 3))))
        (reload buffer)))))

(defmethod initialize-buffers ((object sgl-hex-grid) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (hex-grid) object
    (with-slots (hg:radius hg:hex-count) hex-grid
      (let* ((coords
               (apply
                #'concatenate 'list
                (loop
                  :for i :from (- hg:radius) :below (1+ hg:radius)
                  :collecting
                  (loop
                    :for j :from (- hg:radius) :below (1+ hg:radius)
                    :when (< (+ (* i i) (* j j))
                             (* hg:radius hg:radius))
                      :collect
                      (hg:center (hg:to-axial (hg:oddr :col i :row j)))))))
             (radii
               (loop
                 :for i :below (length coords)
                 :collecting 1.0))
             (states
               (loop
                 :for i :below (length coords)
                 :collecting (mod (logand i (+ 2 (* i i))) 3)))
             (indices
               (loop
                 :for i :below (length coords)
                 :collecting i))

             (c-pointer (sgl:to-gl-array :float (* 2 (length coords)) coords))
             (s-pointer (sgl:to-gl-array :int (length states) states))
             (r-pointer (sgl:to-gl-array :float (length radii) radii))
             (i-pointer (to-gl-array :unsigned-int (length indices) indices)))

        (setf hg:hex-count (length coords))

        (set-buffer object
                    :vertices
                    (make-instance
                     'attribute-buffer
                     :pointer c-pointer
                     :stride nil
                     :attributes '(("in_position" . :vec2))
                     :usage :dynamic-draw
                     :free t))

        (set-buffer object
                    :states
                    (make-instance
                     'attribute-buffer
                     :pointer s-pointer
                     :stride nil
                     :attributes '(("state" . :int))
                     :usage :dynamic-draw
                     :free nil))

        (set-buffer object
                    :radii
                    (make-instance
                     'attribute-buffer
                     :pointer r-pointer
                     :stride nil
                     :attributes '(("radius" . :float))
                     :usage :static-draw
                     :free t))


        (set-buffer object
                    :indices
                    (make-instance
                     'index-buffer
                     :idx-count (length coords)
                     :pointer i-pointer
                     :stride nil
                     :usage :static-draw
                     :free t))))))

;; (defmethod render :before ((object sgl-h3-disk))
;;   (with-slots (buffers uniforms primitive-type style) object
;;     (bind object)
;;     (use-style style)

;;     (gl:polygon-mode :front-and-back
;;                      :line)))
