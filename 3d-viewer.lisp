;; 3d-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass 3d-viewer (viewer)
  ((pos :initform (vec3 0 0 -5)
        :initarg :pos
        :type vec3
        :documentation "Camera position.")
   (quat :initform (vec4 0 0 0 1.0)
        :initarg :pos
        :type vec4
        :documentation "Orientation quaternion (x,y,z,w) w:real xyz: imaginary")
   (view-xform :initform (mperspective 80.0 1.0 0.1 100)
              :initarg :view-xform
              :type mat4
              :documentation "Default view matrix.")
   (rotation :initform t
             :type (or t nil)
             :initarg :rotation)
   (zoom :initform t
         :type (or t nil)
         :initarg :zoom))

  (:documentation "A viewer with keyboard and 3d mouse camera rotation around a target point."))


(defun mat4-quat (quat)
  (let* ((xsq2 (* 2 (vx quat) (vx quat)))
         (ysq2 (* 2 (vy quat) (vy quat)))
         (zsq2 (* 2 (vz quat) (vz quat)))
         (sx (- 1 ysq2 zsq2))
         (sy (- 1 xsq2 zsq2))
         (sz (- 1 xsq2 ysq2)))
    (with-vec4 (x y z w) quat
      (mat4 (list  sx ;; 0 
                   (* 2 (+ (* x y) (* w z))) ;; 1
                   (* 2 (+ (* z x) (* -1 w y))) ;; 2
                   0 ;; 3
                   (* 2 (+ (* x y) (* -1 w z))) ;; 4
                   sy ;; 5
                   (* 2 (+ (* y z) (* w x))) ;; 6
                   0 ;;7
                   (* 2 (+ (* z x) (* w y))) ;; 8
                   (* 2 (+ (* y z) (* -1 w x))) ;; 9
                   sz ;; 10
                   0 ;; 11
                   0 ;; 12
                   0 ;; 13
                   0 ;; 14
                   1.0) ;; 15
            ))))

(defmethod view-matrix ((viewer 3d-viewer))
  (with-slots (quat pos view-xform) viewer
    (m* view-xform
        (3d-matrices:mtranslation pos)        
        (mat4-quat quat)

        )))

(defmethod reset-view-safe (viewer)
  (with-slots (view-changed objects pos quat) viewer
    (setf pos (vec3 0 0 -5))
    (setf quat (vec4 0 0 0.0 1))

    (loop
      :with view-xform = (view-matrix viewer)
      :for (nil . object) :in objects
      :do
         (ensure-initialized object)
         (set-uniform object "view_transform" view-xform :mat4))
    (setf view-changed t)))

(defmethod handle-key ((viewer 3d-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio rotation zoom pos quat view-changed) viewer
      (let* (
             (x 0)
             (y 0)
             (z 0)
             (rx 0)
             (ry 0)
             (rz 0)
             )

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
            (when (and rotation
                       (not (zerop len)))
              (setf quat (quaternion-rotate quat
                                           (* len 0.01)
                                           (vec3 (/ rx len -1)
                                                 (/ ry len -1)
                                                 (/ rz len 1)))))
            (when zoom
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
  (let ((half (* 0.5 angle))
        (sin-half (sin (* 0.5 angle))))
    (quat-mul quaternion
              (vec4 (* (vx xyz) sin-half)
                    (* (vy xyz) sin-half)
                    (* (vz xyz) sin-half)
                    (cos half))
              )))

(defun quat-mul (a b)
  (let ((dot (v. (vxyz a) (vxyz b)))
        (cross (vc (vxyz a) (vxyz b))))
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
  (let* ((len-sq (3d-vectors:vsqrlength quat))
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
    (vxyz
     (quat-mul
      
      (quat-mul vq quat)
      inv-q
      ))))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (pos quat zoom rotation view-changed) viewer
      (setf view-changed t)
      (with-slots (sn:x sn:y sn:z sn:rx sn:ry sn:rz) event
        
        (let ((len (sqrt (+ (* sn:rx sn:rx)
                            (* sn:ry sn:ry)
                            (* sn:rz sn:rz)))))
          (when (and rotation
                     (not (zerop len)))
            (setf quat (quaternion-rotate quat
                                          (* len 0.00085)
                                          (vec3 (/ sn:rx len 1)
                                                (/ sn:ry len 1)
                                                (/ sn:rz len 1)))))
          (when zoom
            ;; Move along z only...
            ;; (setf pos (vec3 0
            ;;                 0
            ;;                 (min -3.0
            ;;                      (+ (vz pos)
            ;;                         (* sn:z 0.01)))))
            (setf pos (v+ pos (vec3-qrot (vec3 (* sn:x 0.001)
                                               (* sn:z 0.001)
                                               (* sn:y 0.001))
                                         quat)))))))))

(defmethod handle-3d-mouse-event ((viewer 3d-viewer) (event sn:button-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (sgl:view-changed
                 sgl:objects) viewer
      (when (sn:button-press-p event 1)
        (reset-view-safe viewer)
        (setf view-changed t)
        t))))
