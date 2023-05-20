;; 3d-mouse-nav-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(declaim (inline handle-3d-mouse-event))

(defmethod view-matrix ((viewer 3d-mouse-nav-viewer))
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

(defmethod handle-key ((viewer 3d-mouse-nav-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio radius φ θ view-changed) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (angle-inc (* multiplier (/ pi 270)))
             (linear-inc (* multiplier 0.5)))

        (cond

          ((and (eq key :f5) (eq action :press))
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
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (up
                 radius
                 φ
                 θ
                 zoom-factor
                 rotation-factor
                 min-radius
                 max-radius
                 θ-direction
                 φ-direction
                 view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (let* ((linear-scale (- 1.0
                                (* (/ sn:z zoom-factor)
                                   (min 2.0 (log radius)))))

               (radial-scale (/ -1.0 rotation-factor))
               (d-θ (* θ-direction radial-scale sn:ry))
               (d-φ (* φ-direction radial-scale sn:rz)))
          (incf θ d-θ)
          (incf φ d-φ)
          (cond
            ((> θ pi)
             (setf θ (- θ (* 2 pi))))
            ((< θ (- pi))
             (setf θ (+ θ (* 2 pi))))
            ((> φ pi)
             (decf φ (* 2 pi))
             (setf up (v- up))
             ;;(setf θ (- θ pi))
             )
            ((< φ (- pi) )
             (format t "reset φ low~%")
             (incf φ (* 2 pi))
             (setf up (v- up))
             ;;(setf θ (+ θ pi))
             )

            ((> φ (/ pi 2))
             (setf up (vec3 0 1 0)))
            ((< φ (/ pi 2))
             (setf up (vec3 0 -1 0)))

            ;; ((< φ (/ (- pi) 2))
            ;;  (setf up (vec3 0 1 0)))
            ;; ((> φ (/ (- pi) 2))
            ;;  (setf up (vec3 0 -1 0)))
            ;; )
            )
          (setf radius (min (max min-radius
                                 (* radius linear-scale))
                            max-radius))
          (setf view-changed t))))))
