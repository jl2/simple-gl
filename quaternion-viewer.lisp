;; quaternion-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass quaternion-viewer (viewer)
  ((radius :initform 100.0 :type real)
   (quaternion :initform (vec4 0.0 0.0 0.0 0.0) :initarg :quaternion :type vec4)
   (min-radius :initform 0.1 :initarg :min-radius :type real)
   (max-radius :initform 100.0 :initarg :max-radius :type real)
   (zoom-factor :initform 2000.0 :initarg :zoom-factor :type real)
   (rotation-factor :initform 2000.0 :initarg :rotation-factor :type real))
  (:documentation "A viewer with 3d mouse camera navigation."))


(defmethod view-matrix ((viewer quaternion-viewer))
  (with-slots (radius target up) viewer
    (m*
     (mperspective 90.0 1.0 0.1 1000.0)
     (mlookat (vec3 0 0 radius)
              (vec3 0 0 0)
              (vec3 0 1 0)))))

(defmethod handle-key ((viewer quaternion-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio radius view-changed) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (angle-inc (* multiplier (/ pi 270)))
             (linear-inc (* multiplier 0.5)))
        (declare (ignorable angle-inc))
        (cond
          ((and (eq key :f5) (eq action :press))
           (reset-view-safe viewer)
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
          (t
           nil)))))

  ;; TODO: Figure out a way to avoid locking the mutex a second time here,
  ;;       preferably without using a recursive lock.
  (call-next-method))


#+spacenav
(defmethod handle-3d-mouse-event ((viewer quaternion-viewer) (event sn:motion-event))
  (with-viewer-lock (viewer)
    (with-slots (radius
                 quaternion
                 zoom-factor rotation-factor
                 view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (let* ((linear-scale (+ 1.0
                                (* (/ sn:y zoom-factor)
                                   (min 2.0 (log radius)))))
               (radial-scale (/ 1.0 rotation-factor)))
          (declare (ignorable linear-scale radial-scale))
          (setf view-changed t))))))
