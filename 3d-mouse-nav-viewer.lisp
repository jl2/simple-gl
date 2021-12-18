;; 3d-mouse-nav-viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass 3d-mouse-nav-viewer (viewer)
  ((radius :initform *default-radius*)
   (theta :initform *default-theta*)
   (gamma :initform *default-gamma*)
   (view-xform :initform (view-matrix *default-radius* *default-theta* *default-gamma*)
               :type mat4))
  (:documentation "A viewer with 3d mouse camera navigation."))


#+spacenav
(defmethod handle-3d-mouse-event ((viewer viewer) (event sn:motion-event))
  (with-slots (objects) viewer
    (dolist (object objects)
      (handle-3d-mouse-event object event))))

(defmethod handle-key ((viewer 3d-mouse-nav-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
    (let* ((multiplier (if (find :shift mod-keys) 4 1.25))
           (angle-inc (* multiplier (/ pi 180)))
           (linear-inc (* multiplier 8.5)))

      (cond

        ((and (eq key :f5) (eq action :press))
         (reset-view viewer)
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
         (setf view-changed t))

        (t
         (call-next-method)))
      (setf view-xform (view-matrix radius theta gamma))
      t)))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
    (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
      (let* ((linear-scale (/ 1.0 (/ radius 4)))
             (radial-scale (/ 1.0 400))
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
        (setf view-changed t)))))
