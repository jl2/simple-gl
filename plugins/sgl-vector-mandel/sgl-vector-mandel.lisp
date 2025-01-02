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

(defparameter *target-steps* 100)

(defclass sgl-vector-mandel (opengl-object)
  ((primitive-type :initform :points)
   (width :initform 128 :initarg :width)
   (height :initform 128 :initarg :height)
   (min-corner :initform (vec2 -1.0f0 -1.0f0) :initarg :min-corner)
   (max-corner :initform (vec2 1.0f0 1.0f0) :initarg :max-corner)
   (position :initform (vec2 0.0 0.0) :initarg :position)
   (power :initform 2.0 :initarg :power)
   
   (target-position :initform (vec2-random -0.5 0.5) :initarg :target-position)
   (target-power :initform (+ 0.1 (random 5.0)) :initarg :target-power)
   (steps-to-target :initform *target-steps* :initarg :steps-to-target)
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
  (sn:sensitivity 0.0125d0)
  (with-slots (sn:y sn:x sn:z) event
    (with-slots (power position) object
      (setf position (v+ (vec2 (* 0.014 sn:x) (* 0.014 sn:z)) position))
      (setf power (min 10.0 (max -10.0 (+ power (* 0.004 sn:y)))))
      (format t "c = ~a power: ~a~%" position power)
      (set-uniform object "c" position :vec2)
      (set-uniform object "power" power :float))))

#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object sgl-vector-mandel) (event sn:button-event))
  (with-slots (power position target-position target-power steps-to-target) object
    (cond
      ((sn:button-press-p event :fit)
       (setf position (vec2 0.0 0.0))
       (setf power 2.0)
       (setf target-position (vec2-random -0.5 0.5))
       (setf target-power (rb 1.5 2.5))
       (setf steps-to-target *target-steps*)
       (set-uniform object "power" power :float)
       (set-uniform object "c" position :vec2)))))



(defun create-vector-mandel (width height min-corner max-corner)
  (declare (type fixnum width height)
           (type vec2 min-corner max-corner))
  (let ((obj (make-instance 'sgl-vector-mandel
                             :width width
                             :height height
                             :max-corner max-corner
                             :min-corner min-corner
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
  (with-slots (power position target-position target-power steps-to-target) object
    (when (zerop steps-to-target)
      (setf target-position (vec2-random -3.14 3.14)
            target-power (rb -10.0 10.0)
            steps-to-target *target-steps*)
      (format t "c = ~a power: ~a~%" position power)
      (format t "target = ~a target-power: ~a steps: ~a~%" target-position target-power steps-to-target))
    (setf position (v+ position  (v/ (v- target-position position)
                                    steps-to-target)))
    (setf power (+ power (/ (- target-power power)
                            steps-to-target)))
    (decf steps-to-target)
    ;; (format t "c = ~a power: ~a~%" position power)
    (set-uniform object "c" position :vec2)
    (set-uniform object "power" power :float))
  nil)
  
