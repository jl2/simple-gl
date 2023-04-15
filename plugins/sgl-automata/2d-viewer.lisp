;; 2d-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass 2d-viewer (viewer)
  ((center :initform (vec3 0 0 0) :type vec3)
   (radius :initform *default-radius* :type real)
   (rotation :initform 0.0 :initarg :rotation :type real))
  (:documentation "A viewer with 3d mouse camera navigation."))


(defmethod view-matrix ((viewer 3d-mouse-nav-viewer))
  (with-slots (radius theta gamma target up) viewer
    (let*  ((tval (* radius (cos theta)))
            (yv (* radius (sin theta)))
            (xv (* tval (cos gamma)))
            (zv (* tval (sin gamma))))
      (m*
       (mperspective 90.0 1.0 0.1 1000.0)
       (mlookat (vec3 xv yv zv)
                target
                up)))))

(defmethod reset-view-safe (viewer)
  (with-slots (view-changed objects aspect-ratio radius theta gamma) viewer
    (setf radius *default-radius*)
    (setf theta *default-theta*)
    (setf gamma *default-gamma*)
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
    (with-slots (aspect-ratio radius theta gamma view-changed) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (angle-inc (* multiplier (/ pi 270)))
             (linear-inc (* multiplier 0.5)))

        (cond

          ((and (eq key :f5) (eq action :press))
           (reset-view-safe viewer)
           (setf view-changed t)
           t)


          ((and (eq key :left) (find action '(:repeat :press)))
           (setf gamma (+ gamma angle-inc))
           (setf view-changed t)
           t)

          ((and (eq key :right) (find action '(:repeat :press)))
           (setf gamma
                 (- gamma angle-inc))
           (setf view-changed t)
           t)


          ((and (eq key :up) (find action '(:repeat :press)))
           (setf theta (min
                        (/ pi 2)
                        (+ theta angle-inc)))
           (setf view-changed t)
           t)

          ((and (eq key :down) (find action '(:repeat :press)))
           (setf theta (max
                        (- (/ pi 2))
                        (- theta angle-inc)))
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
                 theta
                 gamma
                 zoom-factor
                 rotation-factor
                 min-radius
                 max-radius
                 gamma-direction
                 theta-direction
                 view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (let* ((linear-scale (- 1.0
                                (* (/ sn:z zoom-factor)
                                   (min 2.0 (log radius)))))

               (radial-scale (/ -1.0 rotation-factor))
               (d-gamma (* gamma-direction radial-scale sn:ry))
               (d-theta (* theta-direction radial-scale sn:rz)))
          (incf gamma d-gamma)
          (incf theta d-theta)
          (cond
            ((> gamma pi)
             (setf gamma (- gamma (* 2 pi))))
            ((< gamma (- pi))
             (setf gamma (+ gamma (* 2 pi))))
            ((> theta pi)
             (decf theta (* 2 pi))
             (setf up (v- up))
             ;;(setf gamma (- gamma pi))
             )
            ((< theta (- pi) )
             (format t "reset theta low~%")
             (incf theta (* 2 pi))
             (setf up (v- up))
             ;;(setf gamma (+ gamma pi))
             )

            ((> theta (/ pi 2))
             (setf up (vec3 0 1 0)))
            ((< theta (/ pi 2))
             (setf up (vec3 0 -1 0)))

            ;; ((< theta (/ (- pi) 2))
            ;;  (setf up (vec3 0 1 0)))
            ;; ((> theta (/ (- pi) 2))
            ;;  (setf up (vec3 0 -1 0)))
            ;; )
            )
          (setf radius (min (max min-radius
                                 (* radius linear-scale))
                            max-radius))
          (setf view-changed t))))))
