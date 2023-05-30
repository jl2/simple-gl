;; 2d-viewer.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defparameter *default-center* (vec2 0.0 0.0))

(defclass 2d-viewer (viewer)
  ((center-pt :initform (vec2 0 0) :type vec2 :initarg :center-pt)
   (θ :initform 0.0f0 :type real :initarg :theta)
   (radius :initform 100 :type number :initarg :radius)
   (rotation :initform t :type (or t nil) :initarg :rotation)
   (zoom :initform t :type (or t nil) :initarg :zoom)
   (pan :initform t :type (or t nil) :initarg :pan))
  (:documentation "A 2d viewer with panning and rotation using the 3d mouse and the keyboard."))


(defmethod sgl::view-matrix ((viewer 2d-viewer))
  (with-slots (center-pt radius θ) viewer
    (m*
        (mortho (- (vx center-pt) radius)
                (+ (vx center-pt) radius)
                (- (vy center-pt) radius)
                (+ (vy center-pt) radius)
                (- radius) radius)
        (mrotation (vec3 0 0 1) θ)
        )))

(defmethod sgl::reset-view-safe ((viewer 2d-viewer))
  (with-slots (sgl:view-changed center-pt radius θ) viewer
    (setf center-pt (vec2 0 0)
          θ 0.0f0
          radius 100
          view-changed t)))

(defmethod sgl::handle-key ((viewer 2d-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (sgl::with-viewer-lock (viewer)
    (with-slots (center-pt radius zoom pan sgl:view-changed sgl:objects ) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (offset (/ (* multiplier radius)
                        30.0)))
        (prog1
            (cond

              ((and (eq key :r) (find action '(:repeat :press)))
               (sgl::reset-view-safe viewer)
               (setf sgl:view-changed t)
               t)

              ((and pan (eq key :left) (find action '(:repeat :press)))
               (decf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and pan (eq key :right) (find action '(:repeat :press)))
               (incf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)


              ((and pan (eq key :up) (find action '(:repeat :press)))
               (incf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and pan (eq key :down) (find action '(:repeat :press)))
               (decf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and zoom (eq key :page-down) (find action '(:repeat :press)))
               (setf radius (+ multiplier radius))
               (setf sgl:view-changed t)
               t)

              ((and zoom (eq key :page-up) (find action '(:repeat :press)))
               (setf radius (max 1
                                 (- radius multiplier)))
               (setf sgl:view-changed t)
               t)

              (t nil))
          (update-all-view-transforms-safe viewer)))))
  (call-next-method))


#+spacenav
(defmethod sgl:handle-3d-mouse-event ((viewer 2d-viewer) (event sn:motion-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (center-pt
                 radius
                 θ
                 rotation
                 zoom
                 pan
                 sgl:view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event

        (setf center-pt (if pan
                             (v+ center-pt
                                 (vec2 (* radius (/ sn:x 2000.0))
                                       (* radius (/ sn:z 2000.0))))
                             center-pt)
              radius (if zoom
                         (max 1.0
                              (+ radius (* radius
                                           (/ sn:y
                                              1200.0))))
                         radius)
              θ (if rotation
                    (mod (+ θ
                            (/ sn:ry
                               2000.0))
                         (* 4 pi))
                    θ)
              sgl:view-changed t
              )))))
