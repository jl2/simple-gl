;; sgl-hex-grid.lisp
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

(in-package :sgl-hex-grid)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-hex-grid "shaders/") sgl:*shader-dirs*))

(defclass sgl-hex-grid (sgl:opengl-object)
  ((hex-grids :initform (make-array 2
                                    :element-type 'hg:hex-grid
                                    :initial-contents (list (hg:make-hex-grid)
                                                            (hg:make-hex-grid)))
              :initarg :hex-grids)
   (state-idx :initform 0)
   (sgl:primitive-type :initform :points)
   (y-coord :initform 0.0 :initarg :y-coord)
   (hex-radius :initform 1.0 :initarg :hex-radius)
   (sgl:styles :initarg :styles
           :initform (list (cons :hex-grid
                                 (make-instance
                                  'sgl:style
                                  ;; :shaders (list (sgl:read-shader "dumb-vertex.glsl")
                                  ;;                (sgl:read-shader "dumb-geometry.glsl")
                                  ;;                (sgl:read-shader "dumb-fragment.glsl"))
                                  :shaders (list (sgl:read-shader "hex-vertex.glsl")
                                                 (sgl:read-shader "hex-geometry.glsl")
                                                 (sgl:read-shader "hex-fragment.glsl"))
                                  :poly-mode :fill)))))
  (:documentation "A simple-gl hexagon grid."))

(defun next-state-idx (shg)
  (with-slots (state-idx) shg
    (mod (1+ state-idx) 2)))

(defun swap-states (shg)
  (with-slots (state-idx) shg
    (setf state-idx (next-state-idx shg))))

(defun make-sgl-hex-grid (&key
                            (initial-hex-grid (hg:make-hex-grid :min-hex (hg:oddr :col -32 :row -32)
                                                                :max-hex (hg:oddr :row 32 :col 32)))
                            (y-coord 0.0)
                            (hex-radius 1.0))
  (with-slots (hg:hex-type hg:default-state hg:min-hex hg:max-hex hg:width) initial-hex-grid
    (let ((hg-copy (hg:make-hex-grid :min-hex hg:min-hex
                                     :max-hex hg:max-hex
                                     :hex-type hg:hex-type
                                     :default-state hg:default-state)))
      (make-instance 'sgl-hex-grid
                     :hex-grids (make-array 2
                                            :element-type 'hg:hex-grid
                                            :initial-contents
                                            (list initial-hex-grid
                                                  hg-copy))
                     :hex-radius hex-radius
                     :y-coord y-coord))))

(defmethod sgl:initialize-uniforms ((object sgl-hex-grid) &key)
  (with-slots (y-coord) object
    (let* ((wsize (glfw:get-window-size))
           (ar (/ (cadr wsize) (car wsize)  1.0)))
    (sgl:set-uniform object
                     "obj_transform"
                     (m* (mscaling (vec3 ar 1.0 1.0))
                         (mrotation (vec3 0 0 1) (/ pi 3))
                         (mscaling (vec3 0.18 0.18 1.0)))
                     :mat4)
    (sgl:set-uniform object
                     "y_coordinate"
                     y-coord
                     :float))))

(defmethod sgl:update ((object sgl-hex-grid) elapsed-seconds )
  (sgl:set-uniform object
                   "time"
                   elapsed-seconds
                   :float)

  (with-slots (hex-grids state-idx) object
    (let* ((cur-grid (aref hex-grids state-idx))
           (next-grid (aref hex-grids (next-state-idx object)))
           (state-buffer (sgl:get-buffer object :states))
           (state-ptr (if state-buffer
                          (slot-value state-buffer 'sgl:pointer)
                          nil))
           (radii-buffer (sgl:get-buffer object :radii))
           (radii-ptr (if radii-buffer
                          (slot-value radii-buffer 'sgl:pointer)
                          nil))
           (coord (copy-structure (hg:min-hex cur-grid)))
           (cur-idx 0))
      (unless (and state-ptr radii-ptr)
        (format t "state-ptr was nil...~%")
        (error "state-ptr was nil."))
      (loop
        :while (< (hg:oddr-col coord) (hg:max-col cur-grid))
        :do
           (setf (hg:oddr-row coord) (hg:min-row cur-grid))
           (loop
             :while (< (hg:oddr-row coord) (hg:max-row cur-grid))
             :for pt = (hg:center coord)
             :do
                (let* ((cur (hg:state cur-grid coord))
                       (neighbors (hg:neighbors coord))
                       (neighbor-sum (loop
                                       :for neigh :across neighbors
                                       :summing
                                       (if (= 1 (hg:state
                                                 cur-grid neigh))
                                           1
                                           0))))

                  (cond
                    ((and (or (zerop cur) (= 2 cur))
                          (= 2 neighbor-sum))
                     (setf (hg:state next-grid coord) 1))
                    ((and (= 1 cur)
                          (or
                           (= 2 neighbor-sum)
                           (= 5 neighbor-sum)
                           (= 6 neighbor-sum)))
                     (setf (hg:state next-grid coord) 1))
                    ((= 1 cur)
                     (setf (hg:state next-grid coord) 4))
                    (t
                     (setf (hg:state next-grid coord) 0)))
                  (setf (gl:glaref state-ptr cur-idx) (hg:state cur-grid coord))
                  (incf cur-idx)
                  (incf (hg:oddr-row coord))))
           (incf (hg:oddr-col coord)))
      (hg:swap-states object)
      (make-instance 'sgl:buffer-reloader :buffers (list radii-buffer state-buffer)))))

(defmethod sgl:initialize-buffers ((object sgl-hex-grid) &key)
  (when (sgl:buffers object)
    (error "Object buffers already setup!"))
  (with-slots (state-idx hex-grids hex-radius) object
    (let* ((cur-grid (aref hex-grids state-idx))
           (coords (gl:alloc-gl-array :float (* 2 (hg:hex-count cur-grid))))
           (states (gl:alloc-gl-array :int (hg:hex-count cur-grid)))
           (radii (gl:alloc-gl-array :float (hg:hex-count cur-grid)))
           (indices (gl:alloc-gl-array :int (hg:hex-count cur-grid)))
           (coord (copy-structure (hg:min-hex cur-grid)))
           (cur-idx 0))
      (loop
        :while (< (hg:oddr-col coord) (hg:max-col cur-grid))
        :do
           (setf (hg:oddr-row coord) (hg:min-row cur-grid))
           (loop
             :while (< (hg:oddr-row coord) (hg:max-row cur-grid))
             :for pt = (hg:center coord hex-radius)
             :do
                (setf (hg:state cur-grid coord) (random 2))
                (setf (gl:glaref indices cur-idx) cur-idx)
                (setf (gl:glaref radii cur-idx) hex-radius)
                (setf (gl:glaref coords (* 2 cur-idx)) (vx pt))
                (setf (gl:glaref coords (1+ (* 2 cur-idx))) (vy pt))
                (setf (gl:glaref states cur-idx) (hg:state cur-grid coord))
                (incf (hg:oddr-row coord))
                (incf cur-idx))
           (incf (hg:oddr-col coord)))
      (sgl:set-buffer object
                  :vertices
                  (make-instance 'sgl:attribute-buffer
                                 :pointer coords
                                 :stride nil
                                 :attributes '(("in_position" . :vec2))
                                 :usage :dynamic-draw
                                 :free t))

      (sgl:set-buffer object
                  :radii
                  (make-instance 'sgl:attribute-buffer
                                 :pointer radii
                                 :stride nil
                                 :attributes '(("radius" . :float))
                                 :usage :dynamic-draw
                                 :free nil))

      (sgl:set-buffer object
                  :states
                  (make-instance 'sgl:attribute-buffer
                                 :pointer states
                                 :stride nil
                                 :attributes '(("state" . :int))
                                 :usage :static-draw
                                 :free nil))

      (sgl:set-buffer object
                  :indices
                  (make-instance 'sgl:index-buffer
                                 :idx-count (hg:hex-count cur-grid)
                                 :pointer indices
                                 :stride nil
                                 :usage :static-draw
                                 :free t)))))

(defmethod handle-key ((object sgl-hex-grid) window key scancode action mod-keys)
  (declare (ignorable object window key scancode action mod-keys))
  (cond
    ((and (eq key :9) (eq action :press))
     (with-slots (state-idx hex-grids) object
       (let* ((cur-grid (aref hex-grids state-idx))
              (state-buffer (sgl:get-buffer object :states))
              (state-ptr (if state-buffer
                             (slot-value state-buffer 'pointer)
                             nil))
              (coord (copy-structure (hg:min-hex cur-grid)))
              (cur-idx 0))
         (loop
           :while (< (hg:oddr-col coord) (hg:max-col cur-grid))
           :do
              (setf (hg:oddr-row coord) (hg:min-row cur-grid))
              (loop
                :while (< (hg:oddr-row coord) (hg:max-row cur-grid))
                :for pt = (hg:center coord)
                :do
                   (setf (hg:state cur-grid coord) (random 3))
                     (setf (gl:glaref state-ptr cur-idx) (hg:state cur-grid coord))
                     (incf (hg:oddr-row coord))
                     (incf cur-idx))
                (incf (hg:oddr-col coord)))
           t)))
    (t nil)))
