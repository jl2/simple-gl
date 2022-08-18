;; 3d-mouse-nav-viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defparameter *default-radius* 120.0)
(defparameter *default-theta* (/ pi 2))
(defparameter *default-gamma* (/ pi 6))

(defun view-matrix (radius theta gamma)
  (let*  ((tval (* radius (cos theta)))
          (yv (* radius (sin theta)))
          (xv (* tval (cos gamma)))
          (zv (* tval (sin gamma))))
  (m* (mperspective 50.0 1.0 0.1 1000.0)
      (mlookat (vec3 xv
                     yv
                     zv)
               (vec3 0 0 0)
               (vec3 0 1 0)))))
(declaim (inline handle-3d-mouse-event))

(defclass 3d-mouse-nav-viewer (viewer)
  ((radius :initform *default-radius*)
   (theta :initform *default-theta*)
   (gamma :initform *default-gamma*)
   (view-xform :initform (view-matrix *default-radius* *default-theta* *default-gamma*)
               :type mat4))
  (:documentation "A viewer with 3d mouse camera navigation."))

(defun reset-view-safe (viewer)
  (with-slots (view-changed objects aspect-ratio radius theta gamma view-xform) viewer
    (setf radius *default-radius*)
    (setf theta *default-theta*)
    (setf gamma *default-gamma*)
    (setf view-xform (view-matrix radius theta gamma))
    (loop
      :for (nil . object) :in objects
      :do
         (ensure-initialized object)
         (set-uniform object "view_transform" view-xform :mat4))
    (setf view-changed t)))

(defmethod reset-view ((viewer 3d-mouse-nav-viewer))
  (with-viewer-lock (viewer)
    (reset-view-safe viewer)))


#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (ensure-initialized object)
         (handle-3d-mouse-event object event))))

(defmethod handle-key ((viewer 3d-mouse-nav-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
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
           (setf view-changed t))

          ((and (eq key :right) (find action '(:repeat :press)))
           (setf gamma
                 (- gamma angle-inc))
           (setf view-changed t))


          ((and (eq key :up) (find action '(:repeat :press)))
           (setf theta (min
                        (/ pi 2)
                        (+ theta angle-inc)))
           (setf view-changed t))

          ((and (eq key :down) (find action '(:repeat :press)))
           (setf theta (max
                        (- (/ pi 2))
                        (- theta angle-inc)))
           (setf view-changed t))


          ((and (eq key :page-up) (find action '(:repeat :press)))
           (setf radius (max 0.5 (- radius linear-inc)))
           (setf view-changed t))

          ((and (eq key :page-down) (find action '(:repeat :press)))
           (setf radius (min 1000.0 (+ radius linear-inc)))
           (setf view-changed t)))
        (setf view-xform (view-matrix radius theta gamma)))))

  ;; TODO: Figure out a way to avoid locking the mutex a second time here,
  ;;       preferably without using a recursive lock.
  (call-next-method))


#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (let* ((linear-scale (/ 1.0 (/ radius 8)))
               (radial-scale (/ 1.0 600))
               (linear-inc (* -1.0 linear-scale sn:z))
               (xang (* -1.0 radial-scale sn:rx))
               (yang (* -1.0 radial-scale sn:ry))
               )
          (incf gamma yang)
          ;;(incf theta xang)
          (setf theta (max (- (/ pi 2))
                           (min (/ pi 2)
                                (+ theta xang))))
          (setf radius (max 0.5
                            (+ radius linear-inc)))
          (setf view-xform (view-matrix radius theta gamma))
          (setf view-changed t))))))
