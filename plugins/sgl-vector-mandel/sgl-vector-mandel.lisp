;; sgl-vector-mandel.lisp
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

(in-package :sgl-vector-mandel)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-vector-mandel "shaders/") sgl:*shader-dirs*))

(defparameter *radius* 2.8)
(defparameter *pow-radius* 3.8)

(defun random-mandel-position (crad prad)
  
  (let ((rv (vec3-random (- crad) crad)))
    (setf (vz rv) (if (zerop prad)
                      0.0
                      (- (random (* 2 prad))
                         prad)))
    rv))

(defclass sgl-vector-mandel (opengl-object)
  ((primitive-type :initform :points)

   (width :initform 128 :initarg :width)
   (height :initform 128 :initarg :height)
   (depth :initform 1 :initarg :depth)

   (min-corner :initform (vec3 -1.95 -1.95 0.0)
               :initarg :min-corner)
   (max-corner :initform (vec3 1.95 1.95 0.0)
               :initarg :max-corner)

   (position :initform (vec3 0.0 0.0 2.0)
             :initarg :position :type vec3)
   
   (target-position :initform (random-mandel-position *radius* *pow-radius*)
                    :initarg :target-position)
   (target-step :initform (vec3 190 190 220)
                :initarg :target-step)
   (current-step :initform (vec3 190 190 220))
   (sgl:styles :initform (list (cons :vector-mandel 
                                     (make-instance 'style
                                                    :shaders (list (sgl:read-shader "vm.vert")
                                                                   (sgl:read-shader "vm.frag")
                                                                   (sgl:read-shader "vm.geom"))))))
   ))

(defmethod initialize-uniforms ((object opengl-object) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4)
  (set-uniform object "view_transform" (meye 4) :mat4)
  (set-uniform object "position" (slot-value object 'position) :vec3)
  t)

(defmethod sgl:initialize-buffers ((object sgl-vector-mandel) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (width height depth max-corner min-corner) object
    (let* ((step (v/ (v- max-corner
                         min-corner)
                    (vec3  (if (= width 1)
                                 1
                                 (1- width))
                          (if (= height 1)
                              1
                              (1- height))
                          (if (= depth 1)
                              1
                              (1- depth)))))
           (data (loop
                   :for i :below width
                   :nconcing
                   (loop
                     :for j :below height
                     :nconcing
                     (loop :for k :below depth
                           :collecting
                           (v+ min-corner
                               (v* step (vec3 i j k))))))))
      (set-buffer object
                :vertices
                (make-instance
                 'attribute-buffer
                 :pointer (to-gl-array
                           :float
                           (* 3 width height depth)
                           data)
                 :stride nil
                 :attributes '(("in_position" . :vec3))
                 :usage :static-draw
                 :free nil))
      (set-buffer object
                  :indices
                  (sgl:constant-index-buffer (* width height depth) :free nil)))))

#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object sgl-vector-mandel) (event sn:motion-event))
  (sn:sensitivity 1.0d0)
  (with-slots (sn:y sn:x sn:z) event
    (with-slots (position) object
      (let ((rv (v+ (vec3 (* 0.000024 sn:x)
                          (* 0.000024 sn:z)
                          (* 0.000024 sn:y))
                    position)))
        (setf position (vec3 (clamp (vx rv) (- *radius*) *radius*)
                             (clamp (vy rv) (- *radius*) *radius*)
                             (clamp (vz rv) (- *pow-radius*) *pow-radius*)
                             ) ))
      (set-uniform object "position" position :vec3))))

#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object sgl-vector-mandel) (event sn:button-event))
  (with-slots (power position target-position current-step target-step) object
    (cond
      ((sn:button-press-p event :menu)
       (setf position (vec3 0.0 0.0 2.0)
             current-step target-step
             target-position (random-mandel-position *radius* *pow-radius*))
       (set-uniform object "position" position :vec3)))))



(defun rb (min max)
  (+ min (random (- max min))))

(defmethod sgl:update ((object sgl-vector-mandel) elapsed-seconds)
  (with-slots (position target-position current-step target-step) object

    (when (zerop (vx current-step))
      (setf (vx target-position) (rb (- *radius*) *radius*)
            (vx current-step) (vx target-step))
      (format t "target: ~a~%tcurrent-step: ~a~%" target-position current-step))

    (when (zerop (vy current-step))
      (setf (vy target-position) (rb (- *radius*) *radius*)
            (vy current-step) (vy target-step))
      (format t "target: ~a~%tcurrent-step: ~a~%" target-position current-step))

    (when (zerop (vz current-step))
      (setf (vz target-position) (rb (- *pow-radius*) *pow-radius*)
            (vz current-step) (vy target-step))
      (format t "target: ~a~%tcurrent-step: ~a~%" target-position current-step))

    (setf position (let* ((rc (vlength (vxy  position)))
                          (rn (vlength (vxy target-position)))
                          (tc (atan (vy position) (vx position)))
                          (tn (atan (vy target-position) (vx target-position)))
                          (dr (- rn rc))
                          (dt (- tn tc))
                          (next-r (+ rc (/ dr
                                           (vx current-step))))
                          (next-t (+ tc (/ dt
                                           (vy current-step)))))
                     (vec3 (* next-r
                              (cos next-t))
                           (* next-r
                              (sin next-t))
                           (+ (vz position)
                              (/ (- (vz target-position)
                                    (vz position))
                                 (vz current-step))))))
    (setf current-step (v- current-step (vec3 1 1 1)))
    (set-uniform object "position" position :vec3))
  nil)
  
