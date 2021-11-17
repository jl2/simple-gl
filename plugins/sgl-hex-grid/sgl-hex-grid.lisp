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

(defclass hex-grid ()
  ((hex-radius :initform 1.0 :initarg :hex-radius)
   (default-state :initform 0 :initarg :default-state)
   (center :initform (vec3 0.0 0.0 0.0) :initarg :center)))

(defclass flat-hex-grid (hex-grid)
  ())

(defclass pointy-hex-grid (hex-grid)
  ())

(deftype axial-address () '(cons number number))


(setf (symbol-function 'q-coord) #'car)
(setf (symbol-function 'r-coord) #'cdr)
(setf (symbol-function 'axial-address) #'cons)

(declaim (ftype (function (axial-address axial-address) (values axial-address &optional)) axial-add))
(defun axial-add (a b)
  (declare (type axial-address a b))
  (axial-address (+ (q-coord a)
                    (q-coord b))
                 (+ (r-coord a)
                    (r-coord b))))

(defgeneric state (hg qr)
  (:documentation "Return the state of cell qr."))

(defmethod state ((hg hex-grid) addr)
  (declare (ignorable addr))
  (slot-value hg 'default-state))

(defun neighbor (address i)
  (declare (type axial-address address)
           (type (integer 0 6) i))
  (let ((offsets '((1 . 0) (1 . -1) (0 . -1)
                   (-1 . 0) (-1 . 1) (0 . 1))))
    (axial-add address (nth i offsets))))

(defun neighbors (address)
  (declare (type axial-address address))
  (let ((offsets '((1 . 0) (1 . -1) (0 . -1)
                   (-1 . 0) (-1 . 1) (0 . 1))))
    (mapcar (curry #'axial-add address) offsets)))

(defgeneric center-coordinate (hg qr)
  )

(defgeneric vertices (hg qr))

(defun center (address &optional (hex-radius 1.0) (hex-type :flat))
  (declare (type axial-address address))
  (let ((q (q-coord address))
        (r (r-coord address)))

    (if (eq :flat hex-type)

        (vec2 (* hex-radius (/ 3 2) q)
              (* hex-radius (+ (* (/ (sqrt 3) 2) q)
                               (* (sqrt 3) r))))

        (vec2 (* hex-radius (+ (* (sqrt 3) q)
                               (* (/ (sqrt 3) 2) r)))
              (* hex-radius (/ 3 2) r)))))

(defun hex-vert (center radius angle num)
  (let ((this-theta (+ angle
                       (/ (* num 2 pi) 6))))
    (v+ center
        (vec2 (* radius (cos this-theta))
              (* radius (sin this-theta))))))

(defclass sgl-hex-grid (opengl-object)
  ((radius :initform 1.0 :initarg :radius)
   (primitive-type :initform :points)
   (hex-type :initform :flat :initarg :hex-type)
   (hex-radius :initform 1.0f0 :initarg :hex-radius)
   (style :initarg :style
          :initform (make-instance
                     'style :name "hexagons"
                            ;; :shaders (list (sgl:read-shader "dumb-vertex.glsl")
                            ;;                (sgl:read-shader "dumb-geometry.glsl")
                            ;;                (sgl:read-shader "dumb-fragment.glsl"))
                            :shaders (list (sgl:read-shader "hex-vertex.glsl")
                                           (sgl:read-shader "hex-geometry.glsl")
                                           (sgl:read-shader "hex-fragment.glsl"))
                            :poly-mode :fill))))

(defmethod initialize-uniforms ((object sgl-hex-grid) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4))

(defun show-gl-array (ptr count)
  (loop
    for i below count do
      (format t "~a " (gl:glaref ptr i)))
  (terpri))

(defmethod update ((object sgl-hex-grid) elapsed-seconds )
  (set-uniform object "time" elapsed-seconds :float))

(defmethod initialize-buffers ((object sgl-hex-grid) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (radius) object
    (let* ((coords ;;(list (vec2 0.0 0.0))
              (apply #'concatenate 'list
              ;;              (list (shg:center (axial-address 0 0)))
              ;;              (mapcar #'shg:center (shg:neighbors (axial-address 0 0))))
              (loop
                for i from (- radius) below (1+ radius)
                collecting (loop for j from (- radius)  below (1+ radius)
                                 when (< (+ (* i i) (* j j))
                                        (* radius radius))
                                   collect
                                   (center (axial-address i j)))))
              )
           (radii (loop
                    for i below (length coords)
                    collecting 1.0))
           (states (loop
                     for i below (length coords)
                     collecting (mod (logand i (+ 2 (* i i))) 3)))
           (indices (loop
                      for i below (length coords)
                      collecting i))

           (c-pointer (sgl:to-gl-array
                             :float
                             (* 2 (length coords))
                             coords))

           (s-pointer (sgl:to-gl-array
                       :int
                       (length states)
                       states))
           (r-pointer (sgl:to-gl-array
                       :float
                       (length radii)
                       radii))
           (i-pointer (to-gl-array
                       :unsigned-int
                       (length indices)
                       indices))
           )
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
                   :usage :static-draw
                   :free t))

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
                   :free t)))))

;; (defmethod render :before ((object sgl-h3-disk))
;;   (with-slots (buffers uniforms primitive-type style) object
;;     (bind object)
;;     (use-style style)

;;     (gl:polygon-mode :front-and-back
;;                      :line)))
