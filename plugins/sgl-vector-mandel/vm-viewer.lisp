;; vm-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass vm-viewer (viewer)
  (
   (pos
    :initform (vec3 0 0 -5)
    :initarg :pos
    :type vec3
    :documentation "Camera position.")

   (field-of-view
    :initform 80.0
    :initarg :field-of-view
    :type real
    :documentation "The view field of view for perspective viewing.")
   (near
    :initform 0.1
    :initarg :near
    :type real
    :documentation "The view's near distance.")
   (far
    :initform 100.0
    :initarg :far
    :type real
    :documentation "The view's far distance.")
   )

  (:documentation "A viewer with keyboard and 3d mouse camera rotation around a target point."))


(defmethod view-matrix ((viewer vm-viewer))
  (with-slots (field-of-view aspect-ratio near far pos) viewer
    (m*
     (mperspective field-of-view aspect-ratio near far)
     (3d-matrices:mtranslation pos))))

(defmethod reset-view-safe (viewer)
  (with-slots (view-changed objects pos) viewer
    (setf pos (vec3 0 0 -5))
    (loop
      :with view-xform :of-type mat4 = (view-matrix viewer)
      :for (nil . object) :in objects
      :do
         (ensure-initialized object)
         (set-uniform object "view_transform" view-xform :mat4))
    (setf view-changed t)))

(defmethod handle-key ((viewer vm-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys)
           (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio cursor-pos view-changed) viewer
      (let* (
             (x 0)
             (y 0)
             (z 0)
             (rx 0)
             (ry 0)
             (rz 0))
        (cond

          ((and (eq key :r) (eq action :press))
           (reset-view-safe viewer)
           (setf view-changed t)
           t)


          ((and (eq key :x)
                (find action '(:repeat :press)))
           (incf rx)
           (setf view-changed t)
           t)

          ((and (eq key :x)
                (find :shift mod-keys)
                (find action '(:repeat :press)))
           (decf rx)
           (setf view-changed t)
           t)


          ((and (eq key :y) (find action '(:repeat :press)))
           (incf ry)
           (setf view-changed t)
           t)

          ((and (eq key :y)
                (find :shift mod-keys)
                (find action '(:repeat :press)))
           (decf ry)
           (setf view-changed t)
           t)

          ((and (eq key :z) (find action '(:repeat :press)))
           (incf rz)
           (setf view-changed t)
           t)

          ((and (eq key :z) (find action '(:repeat :press)))
           (incf rz)
           (setf view-changed t)
           t)

          ((and (eq key :x)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf x)
           (setf view-changed t)
           t)
          ((and (eq key :x)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf x)
           (setf view-changed t)
           t)
          ((and (eq key :y)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf y)
           (setf view-changed t)
           t)
          ((and (eq key :y)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf y)
           (setf view-changed t)
           t)
          ((and (eq key :z)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (incf z)
           (setf view-changed t)
           t)
          ((and (eq key :z)
                (find :shift mod-keys)
                (find :control mod-keys)
                (find action '(:repeat :press)))
           (decf z)
           (setf view-changed t)
           t)


          (t nil))
        (when view-changed
          (let ((len (sqrt
                      (+ (* rx rx)
                         (* ry ry)
                         (* rz rz)))))
            (when (and rotation-enabled
                       (not (zerop len)))
              (setf quat (quaternion-rotate quat
                                           (* len 0.01)
                                           (vec3 (/ rx len -1)
                                                 (/ ry len -1)
                                                 (/ rz len 1)))))
            (when zoom-enabled
              (setf pos (v+ pos
                            (vec3-qrot (vec3 (- x)
                                             (- y)
                                             z)
                                       quat)))))
          t))))

  ;; TODO: Figure out a way to avoid locking the mutex a second time here,
  ;;       preferably without using a recursive lock.
  (call-next-method))

(defun quaternion-rotate (quaternion angle xyz)
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type #.3d-vectors::*float-type* angle)
            (type vec3 xyz))
  (let ((half (* 0.5 angle))
        (sin-half (sin (* 0.5 angle))))
    (quat-mul quaternion
              (vec4 (* (vx xyz) sin-half)
                    (* (vy xyz) sin-half)
                    (* (vz xyz) sin-half)
                    (cos half))
              )))

(defun quat-mul (a b)
 
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 a b))
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0)))
  (let ((dot (v. (vxyz a) (vxyz b)))
        (cross (vc (vxyz a) (vxyz b))))
    (declare  (type #.3d-vectors::*float-type* dot)
             (type vec3 cross))
    (with-vec4 (ax ay az aw) a
      (with-vec4 (bx by bz bw) b
        (with-vec3 (cx cy cz) cross
          (vec4 (+  (* aw bx)
                    (* bw ax)
                    cx)
                (+  (* aw by)
                    (* bw ay)
                    cy)
                (+  (* aw bz)
                    (* bw az)
                    cz)
                (- (* aw bw)
                   dot)))))))

(defun quat-invert (quat)
  (declare  (optimize (speed 3) (safety 0) (debug 0) (space 0))
            (type vec4 quat))
  (let* ((len-sq (vsqrlength quat))
         (scale (if (> len-sq 0.01)
                    (/ 1.0 len-sq)
                    1.0)))

    (vec4 (* scale (- (vx quat)))
          (* scale (- (vy quat)))
          (* scale (- (vz quat)))
          (* scale (vw quat)))))

(defun vec3-qrot (vec quat)
  (let ((inv-q (quat-invert quat))
        (vq (vxyz_ vec)))
    (vxyz (quat-mul
           (quat-mul vq quat)
           inv-q))))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer vm-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (pos
                 quat
                 zoom-enabled
                 rotation-enabled
                 view-changed) viewer
      (setf view-changed t)
      (with-slots (sn:x sn:y sn:z
                   sn:rx sn:ry sn:rz) event

        (let ((len (sqrt (+ (* sn:rx sn:rx)
                            (* sn:ry sn:ry)
                            (* sn:rz sn:rz)))))
          (when (and rotation-enabled
                     (not (zerop len)))
            (setf quat (quaternion-rotate quat
                                          (* len 0.03)
                                          (vec3 (/ sn:rx len -1.0)
                                                (/ sn:ry len -1.0)
                                                (/ sn:rz len 1.0)))))
          (when zoom-enabled
            (setf pos (v+ pos (vec3-qrot (vec3 (* sn:x -0.08)
                                               (* sn:y -0.08)
                                               (* sn:z 0.08))
                                         quat)))))))))

(defmethod handle-joystick ((viewer vm-viewer))
  (with-viewer-lock (viewer)
    (with-slots (pos
                 quat
                 zoom-enabled
                 joysticks
                 rotation-enabled
                 view-changed) viewer
      (when (null joysticks)
        (return-from handle-joystick nil))
      (let* ((joystick (etypecase joysticks
                         (list (first joysticks))
                         (fixnum joysticks)))
             (buttons (make-array (length #1=(the list (glfw:get-joystick-buttons joystick))) :element-type 'fixnum :initial-contents #1#))
             (axes (make-array (length #2= (the list (glfw:get-joystick-axes joystick))) :element-type 'single-float :initial-contents #2#)))
        (declare (ignorable buttons))
        ;;        (format t "buttons: ~a axes: ~a~%" buttons axes)
        (setf view-changed t)
        (let ((len (sqrt (+ (* (aref axes 0) (aref axes 0))
                            (* (aref axes 1) (aref axes 1))
                            (* (aref axes 2) (aref axes 2))))))
          (declare (ignorable len))
          (when zoom-enabled
            (setf pos
                  (v+ pos
                      (vec3-qrot (vec3
                                  (* (if (> (abs (aref axes 0)) 0.02)
                                         (aref axes 0)
                                         0.0)
                                     0.2)
                                  (* (if (> (abs (aref axes 2)) 0.02)
                                         (aref axes 2)
                                         0.0)
                                     0.1)
                                  (*  (if (> (abs (aref axes 1)) 0.02)
                                          (aref axes 1)
                                          0.0)
                                      -0.5))
                                 quat)))))))))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer vm-viewer) (event sn:button-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (sgl:view-changed
                 rotation-enabled
                 window
                 sgl:objects) viewer
      (cond
        ((sn:button-press-p event :fit)
         (format t ":fit pressed!~%")
         (reset-view-safe viewer)
         (setf view-changed t)
         t)

        ((sn:button-press-p event :esc)
         (format t ":esc pressed!~%")
         (glfw:set-window-should-close window)
         (setf view-changed t)
         t)

        ((sn:button-press-p event :joystick)
         (format t ":joystick pressed!~%")
         (setf rotation-enabled (not rotation-enabled))
         t)))))