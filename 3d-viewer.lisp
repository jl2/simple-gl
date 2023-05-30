;; 3d-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass 3d-viewer (viewer)
  ((radius :initform 120.0
           :type real
           :documentation "The distance between the camera and the target.")

   (θ :initform (/ pi 2)
      :type real
      :documentation "Angle around the x axis.")
   (φ :initform (/ pi 6)
      :type real
      :documentation "Angle around the z axis.")
   (target :initform (vec3 0.0 0.0 0.0)
           :initarg :target
           :type vec3
           :documentation "")
   (up :initform (vec3 0 1 0)
       :initarg :up
       :type vec3
       :documentation "")
   (min-radius :initform 0.1
               :initarg :min-radius
               :type real
               :documentation "")
   (max-radius :initform 100.0
               :initarg :max-radius
               :type real
               :documentation "")
   (zoom-factor :initform 2000.0
                :initarg :zoom-factor
                :type real
                :documentation "")
   (rotation-factor :initform 2000.0
                    :initarg :rotation-factor
                    :type real
                    :documentation "")
   (rotation :initform t :type (or t nil) :initarg :rotation)
   (zoom :initform t :type (or t nil) :initarg :zoom))

  (:documentation "A viewer with keyboard and 3d mouse camera rotation around a target point."))


(defmethod view-matrix ((viewer 3d-viewer))
  (with-slots (radius φ θ target up) viewer
    (let*  ((tval (* radius (cos φ)))
            (yv (* radius (sin φ)))
            (xv (* tval (cos θ)))
            (zv (* tval (sin θ))))
      (m*
       (mperspective 90.0 1.0 0.1 1000.0)
       (mlookat (vec3 xv yv zv)
                target
                up)))))

(defmethod reset-view-safe (viewer)
  (with-slots (view-changed objects aspect-ratio radius φ θ) viewer
    (setf radius *default-radius*)
    (setf φ *default-φ*)
    (setf θ *default-θ*)
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
    (with-slots (aspect-ratio radius φ θ view-changed) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (angle-inc (* multiplier (/ pi 270)))
             (linear-inc (* multiplier 0.5)))

        (cond

          ((and (eq key :r) (eq action :press))
           (reset-view-safe viewer)
           (setf view-changed t)
           t)


          ((and (eq key :left) (find action '(:repeat :press)))
           (setf θ (+ θ angle-inc))
           (setf view-changed t)
           t)

          ((and (eq key :right) (find action '(:repeat :press)))
           (setf θ
                 (- θ angle-inc))
           (setf view-changed t)
           t)


          ((and (eq key :up) (find action '(:repeat :press)))
           (setf φ (min
                        (/ pi 2)
                        (+ φ angle-inc)))
           (setf view-changed t)
           t)

          ((and (eq key :down) (find action '(:repeat :press)))
           (setf φ (max
                        (- (/ pi 2))
                        (- φ angle-inc)))
           (setf view-changed t)
           t)


          ((and (eq key :page-up) (find action '(:repeat :press)))
           (setf radius (max 0.5 (- radius linear-inc)))
           (setf view-changed t)
           t)

          ((and (eq key :page-down) (find action '(:repeat :press)))
           (setf radius (min 1000.0 (+ radius linear-inc)))
           (setf view-changed t)
           t)
          (t nil)))))

  ;; TODO: Figure out a way to avoid locking the mutex a second time here,
  ;;       preferably without using a recursive lock.
  (call-next-method))


#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (up
                 radius
                 φ
                 θ
                 zoom
                 rotation
                 zoom-factor
                 rotation-factor
                 min-radius
                 max-radius
                 view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (setf radius (if zoom
                         (max 1.0
                              (+ radius (* radius
                                           (/ sn:y
                                              1200.0))))
                         radius)
              θ (if rotation (mod (+ θ
                            (/ sn:rx
                               2000.0))
                                  (* 4 pi))
                    θ)
              φ (if rotation (mod (+ θ
                            (/ sn:rz
                               2000.0))
                                  (* 4 pi))
                    θ)
              view-changed t)))))
