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

(defparameter *position-steps* 300)
(defparameter *power-steps* 260)
(defparameter *radius* 2.5)
(defparameter *max-power* 7.0)
(defclass sgl-vector-mandel (opengl-object)
  ((primitive-type :initform :points)
   (width :initform 128 :initarg :width)
   (height :initform 128 :initarg :height)
   (min-corner :initform (vec2 (- *radius*) (- *radius*)) :initarg :min-corner)
   (max-corner :initform (vec2 *radius* *radius*) :initarg :max-corner)
   (position :initform (vec2 0.0 0.0) :initarg :position)
   (power :initform 2.0 :initarg :power)
   
   (target-position :initform (vec2-random (- *radius*) *radius*) :initarg :target-position)
   (target-power :initform (rb (- *radius*) *radius*) :initarg :target-power)
   (steps-to-power :initform *power-steps* :initarg :steps-to-power)
   (steps-to-position :initform *position-steps* :initarg :steps-to-position)
   ))

(defmethod initialize-uniforms ((object opengl-object) &key)
  (set-uniform object "obj_transform" (meye 4) :mat4)
  (set-uniform object "view_transform" (meye 4) :mat4)
  (set-uniform object "power" (slot-value object 'power) :float)
  (set-uniform object "c" (slot-value object 'position) :vec2)
  t)

(defmethod sgl:initialize-buffers ((object sgl-vector-mandel) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (with-slots (width height depth max-corner min-corner) object
    (let* ((step (v/ (v- max-corner
                         min-corner)
                    (vec2 (if (= width 1)
                              1
                              (1- width))
                          (if (= height 1)
                              1
                              (1- height)))))
          (data (loop
                  :for i :below width
                  :nconcing
                  (loop
                    :for j :below height
                    :collecting
                    (v+
                     min-corner
                     (v* (vec2 i j) step))))))
      (set-buffer object
                :vertices
                (make-instance
                 'attribute-buffer
                 :pointer (to-gl-array
                           :float
                           (* 2 width height)
                           data)
                 :stride nil
                 :attributes '(("in_position" . :vec2))
                 :usage :static-draw
                 :free nil))
      (set-buffer object
                  :indices
                  (sgl:constant-index-buffer (* width height) :free nil)))))
#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object sgl-vector-mandel) (event sn:motion-event))
  (sn:sensitivity 1.0d0)
  (with-slots (sn:y sn:x sn:z) event
    (with-slots (power position) object
      (setf position (v+ (vec2 (* 0.000124 sn:x) (* 0.000124 sn:z)) position))
      (setf power (min *max-power* (max (- *max-power*) (+ power (* 0.000124 sn:y)))))
      (format t "c = ~a power: ~a~%" position power)
      (set-uniform object "c" position :vec2)
      (set-uniform object "power" power :float))))

#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object sgl-vector-mandel) (event sn:button-event))
  (with-slots (power position target-position target-power steps-to-power steps-to-position) object
    (cond
      ((sn:button-press-p event :fit)
       (setf position (vec2 0.0 0.0))
       (setf power 2.0)
       (setf steps-to-position *position-steps*)
       (setf steps-to-power *power-steps*)
       (set-uniform object "power" power :float)
       (set-uniform object "c" position :vec2)))))



(defun create-vector-mandel (&key (width 120)
                               (height 120)
                               (min-corner (vec2 -1.5 -1.5))
                               (max-corner (vec2 1.5 1.5))
                               (position (vec2 0.0 0.0))
                               (power 2.0)
                               (target-position (vec2-random (- *radius*) *radius*))
                               (target-power (rb (- *radius*) *radius*))
                               (steps-to-position *position-steps*)
                               (steps-to-power *power-steps*)
                               )
  (declare (type fixnum width height steps-to-power steps-to-position)
           (type vec2 min-corner max-corner position target-position)
           (type float power target-power))
  (let ((obj (make-instance 'sgl-vector-mandel
                             :width width
                             :height height
                             :max-corner max-corner
                             :min-corner min-corner
                             :position position
                             :target-position target-position
                             :power power
                             :target-power target-power
                             :steps-to-position steps-to-position
                             :steps-to-power steps-to-power
                             :styles (list (cons :vector-mandel 
                                                 (make-instance 'style
                                                                :shaders (list (sgl:read-shader "vm.vert")
                                                                               (sgl:read-shader "vm.frag")
                                                                               (sgl:read-shader "vm.geom"))))))))
    (sgl:set-uniform obj "diffuse_color" (vec4 0.0 0.8 0.0 0.9) :vec4)
    obj))

(defun rb (min max)
  (+ min (random (- max min))))

(defmethod update ((object sgl-vector-mandel) elapsed-seconds)
  (with-slots (power position target-position steps-to-position target-power steps-to-power) object

    (when (zerop steps-to-power)
      (setf target-power (rb (- *max-power*) *max-power*)
            steps-to-power *power-steps*)
      (format t "power: ~a~%target-power: ~a~%steps: ~a~%" power target-power steps-to-power)
      )

    (when (zerop steps-to-position)
      (setf target-position (let ((rr (random *radius*))
                                  (rt (random (* 2 pi))))
                              (vec2 (* rr (cos rt))
                                    (* rr (sin rt))))
            ;;(vec2-random (- *radius*) *radius*)
            steps-to-position *position-steps*)
      (format t "c = ~a~%target = ~a~%steps: ~a~%" position target-position steps-to-position)
      )

    (setf position (let ((rc (vlength position))
                         (rn (vlength target-position))
                         (tc (atan (vy position) (vx position)))
                         (tn (atan (vy target-position) (vx target-position))))
                     (vec2 (* (+ rc (/ (- rn rc) steps-to-position))
                              (cos (+ tc (/ (- tn tc) steps-to-position))))
                           (* (+ rc (/ (- rn rc) steps-to-position))
                              (sin (+ tc (/ (- tn tc) steps-to-position))))))
          ;; (v+ position
          ;;     (v/ (v- target-position position)
          ;;         steps-to-position))
          )
    (setf power (+ power
                   (/ (- target-power power)
                      steps-to-power)))
    (decf steps-to-position)
    (decf steps-to-power)
    (set-uniform object "c" position :vec2)
    (set-uniform object "power" power :float))
  nil)
  
