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
  ((hex-grid :initform (hg:make-hex-grid) :initarg :hex-grid)
   (min-coord :initform (vec2 -1.0 -1.0) :initarg :min-coord)
   (primitive-type :initform :points)
   (y-coord :initform 0.0 :initarg :y-coord)
   (style :initarg :style
          :initform (make-instance
                     'style
                     :name "hexagons"
                     ;; :shaders (list (sgl:read-shader "dumb-vertex.glsl")
                     ;;                (sgl:read-shader "dumb-geometry.glsl")
                     ;;                (sgl:read-shader "dumb-fragment.glsl"))
                     :shaders (list (sgl:read-shader "hex-vertex.glsl")
                                    (sgl:read-shader "hex-geometry.glsl")
                                    (sgl:read-shader "hex-fragment.glsl"))
                     :poly-mode :fill))))

(defmethod initialize-uniforms ((object sgl-hex-grid) &key)
  (with-slots (y-coord) object
    (set-uniform object "obj_transform" (meye 4) :mat4)
    (set-uniform object "y_coordinate" y-coord :float)))

(defun show-gl-array (ptr count)
  (loop
    for i below count do
      (format t "~a " (gl:glaref ptr i)))
  (terpri))

(defmethod update ((object sgl-hex-grid) elapsed-seconds )
  (set-uniform object "time" elapsed-seconds :float)
  (with-slots ((hg hex-grid)) object
    (with-slots ((state-idx hg:state-idx)) hg
      (let* ((state-buffer (sgl:get-buffer object :states))
             (state-ptr (if state-buffer
                            (slot-value state-buffer 'pointer)
                            nil))
             (radii-buffer (sgl:get-buffer object :radii))
             (radii-ptr (if radii-buffer
                            (slot-value radii-buffer 'pointer)
                            nil))
             (next-state-idx (mod (1+ state-idx) 2))
             (coord (copy-structure (hg:min-hex hg)))
             (cur-idx 0))
        (cond ((and state-ptr radii-ptr)
               (loop
                 while (< (hg:oddr-col coord) (hg:max-col hg))
                 do
                    (setf (hg:oddr-row coord) (hg:min-row hg))
                    (loop
                      while (< (hg:oddr-row coord) (hg:max-row hg))
                      for pt = (hg:center coord)
                      do
                         (let* ((cur (hg:state hg state-idx coord))
                                (neighbors (hg:neighbors coord))
                                (stat (loop
                                        for neigh across neighbors
                                        summing (if (= 1 (hg:state hg state-idx neigh)) 1 0))))
                           (cond ((and (or (zerop cur) (= 2 cur))
                                       (= 2 stat))
                                  (setf (hg:state hg next-state-idx coord) 1)
                                  (let ((diff (- 0.05 (random 0.10))))
                                    (setf (gl:glaref radii-ptr cur-idx)
                                          (+ (gl:glaref radii-ptr cur-idx) diff))))
                                 ((and (= 1 cur)
                                       (or
                                        (= 3 stat)
                                        (= 4 stat)))
                                  (let ((diff (- 0.08 (random 0.16))))
                                    (setf (gl:glaref radii-ptr cur-idx)
                                          (+ (gl:glaref radii-ptr cur-idx) diff)))
                                  (setf (hg:state hg next-state-idx coord) 1))
                                 ((= 1 cur)
                                  (setf (hg:state hg next-state-idx coord) 2))
                                 (t
                                  (setf (hg:state hg next-state-idx coord) 0)))
                           ;; (cond (
                           ;;             (or
                           ;;              (= 2 stat)
                           ;;              (= 4 stat))
                           ;;        (setf (hg:state hg next-state-idx coord) 1))
                           ;;       (t
                           ;;        (setf (hg:state hg next-state-idx coord) 0)))
                           (setf (gl:glaref state-ptr cur-idx) (hg:state hg state-idx coord))
                           (incf cur-idx)
                           (incf (hg:oddr-row coord))))
                    (incf (hg:oddr-col coord)))
               (hg:swap-states hg))
              (t
               (format t "state-ptr was nil...~%")))
        (sgl:reload radii-buffer)
        (sgl:reload state-buffer)))))

(defmethod initialize-buffers ((object sgl-hex-grid) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots ((hg hex-grid)) object
    (with-slots ((state-idx hg:state-idx)) hg
      (let* (
             (coords (gl:alloc-gl-array :float (* 2 (hg:hex-count hg))))
             (states (gl:alloc-gl-array :int (hg:hex-count hg)))
             (radii (gl:alloc-gl-array :float (hg:hex-count hg)))
             (indices (gl:alloc-gl-array :int (hg:hex-count hg)))
             (coord (copy-structure (hg:min-hex hg)))
             (cur-idx 0))
        (loop
          while (< (hg:oddr-col coord) (hg:max-col hg))
          do
             (setf (hg:oddr-row coord) (hg:min-row hg))
             (loop
               while (< (hg:oddr-row coord) (hg:max-row hg))
               for pt = (hg:center coord)
               do
                  (setf (hg:state hg state-idx coord) (random 2))
                  (setf (gl:glaref indices cur-idx) cur-idx)
                  (setf (gl:glaref radii cur-idx) (+ 0.4 (random 0.65)))
                  (setf (gl:glaref coords (* 2 cur-idx)) (vx pt))
                  (setf (gl:glaref coords (1+ (* 2 cur-idx))) (vy pt))
                  (setf (gl:glaref states cur-idx) (hg:state hg state-idx coord))
                  (incf (hg:oddr-row coord))
                  (incf cur-idx))
             (incf (hg:oddr-col coord)))
        (set-buffer object
                    :vertices
                    (make-instance
                     'attribute-buffer
                     :pointer coords
                     :stride nil
                     :attributes '(("in_position" . :vec2))
                     :usage :dynamic-draw
                     :free t))

        (set-buffer object
                    :radii
                    (make-instance
                     'attribute-buffer
                     :pointer radii
                     :stride nil
                     :attributes '(("radius" . :float))
                     :usage :dynamic-draw
                     :free nil))

        (set-buffer object
                    :states
                    (make-instance
                     'attribute-buffer
                     :pointer states
                     :stride nil
                     :attributes '(("state" . :int))
                     :usage :static-draw
                     :free nil))

        (set-buffer object
                    :indices
                    (make-instance
                     'index-buffer
                     :idx-count (hg:hex-count hg)
                     :pointer indices
                     :stride nil
                     :usage :static-draw
                     :free t))))))

(defmethod handle-key ((object sgl-hex-grid) window key scancode action mod-keys)
  (declare (ignorable object window key scancode action mod-keys))
  (when (and (eq key :9) (eq action :press))
    (with-slots ((hg hex-grid)) object
      (with-slots ((state-idx hg:state-idx)) hg
        (let* ((state-buffer (sgl:get-buffer object :states))
               (state-ptr (if state-buffer
                              (slot-value state-buffer 'pointer)
                              nil))
               (coord (copy-structure (hg:min-hex hg)))
               (cur-idx 0))
          (loop
            while (< (hg:oddr-col coord) (hg:max-col hg))
            do
               (setf (hg:oddr-row coord) (hg:min-row hg))
               (loop
                 while (< (hg:oddr-row coord) (hg:max-row hg))
                 for pt = (hg:center coord)
                 do
                    (setf (hg:state hg state-idx coord) (random 3))
                    (setf (gl:glaref state-ptr cur-idx) (hg:state hg state-idx coord))
                    (incf (hg:oddr-row coord))
                    (incf cur-idx))
               (incf (hg:oddr-col coord)))
          t)))))

;; (defmethod render :before ((object sgl-h3-disk))
;;   (with-slots (buffers uniforms primitive-type style) object
;;     (bind object)
;;     (use-style style)

;;     (gl:polygon-mode :front-and-back
;;                      :line)))
