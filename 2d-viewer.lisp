;; 2d-viewer.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defparameter *default-center* (vec2 0.0 0.0))

(defclass 2d-viewer (viewer)
  ((center-pt
    :initform (vec2 0 0)
    :type vec2
    :initarg :center-pt
    :documentation "The point that is transformed to the center of the view.")

   (ϴ
    :initform 0.0f0
    :type real
    :initarg :theta
    :documentation "The current rotation angle around the center.")

   (radius
    :initform 100
    :type number
    :initarg :radius
    :documentation "")

   (rotation-enabled
    :initform t
    :type (or t nil)
    :initarg :rotation-enabled
    :documentation "nil if view rotation is disabled.  non-nil if rotation is enabled.")

   (zoom
    :initform t
    :type (or t nil)
    :initarg :zoom
    :documentation "nil if view zooming is disabled.  non-nil if zooming is enabled.")

   (pan
    :initform t
    :type (or t nil)
    :initarg :pan
    :documentation "nil if view panning is disabled.  non-nil if panning is enabled."))
  (:documentation "A 2d viewer with panning, zooming and rotation using the 3d mouse and the keyboard."))

(defmethod sgl::view-matrix ((viewer 2d-viewer))
  (with-slots (center-pt aspect-ratio width height radius ϴ) viewer
    (m*
     (if (< width height )
         (mortho (- (vx center-pt) radius)
             (+ (vx center-pt) radius)
             (- (vy center-pt) (* aspect-ratio radius))
             (+ (vy center-pt) (* aspect-ratio radius))
             (* -100 radius) (* 100 radius))

         (mortho (- (vx center-pt) (* aspect-ratio radius))
             (+ (vx center-pt) (* aspect-ratio radius))
             (- (vy center-pt) radius)
             (+ (vy center-pt) radius)
             (* -100 radius) (* 100 radius)))
     (mrotation (vec3 0 0 1) (* 2  ϴ))
        )))

(defmethod sgl::reset-view-safe ((viewer 2d-viewer))
  (with-slots (sgl:view-changed center-pt radius ϴ) viewer
    (setf center-pt (vec2 0 0)
          ϴ 0.0f0
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

              ((and (eq key :r)
                    (find action '(:repeat :press)))
               (sgl::reset-view-safe viewer)
               (setf sgl:view-changed t)
               t)

              ((and pan
                    (eq key :left)
                    (find action '(:repeat :press)))
               (decf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and pan
                    (eq key :right)
                    (find action '(:repeat :press)))
               (incf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)


              ((and pan
                    (eq key :up)
                    (find action '(:repeat :press)))
               (incf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and pan
                    (eq key :down)
                    (find action '(:repeat :press)))
               (decf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and zoom
                    (eq key :page-down)
                    (find action '(:repeat :press)))
               (setf radius (+ multiplier radius))
               (setf sgl:view-changed t)
               t)

              ((and zoom
                    (eq key :page-up)
                    (find action '(:repeat :press)))
               (setf radius (max 1
                                 (- radius multiplier)))
               (setf sgl:view-changed t)
               t)

              (t
               nil))
          (update-all-view-transforms-safe viewer)))))
  (call-next-method))


#+spacenav
(defmethod sgl:handle-3d-mouse-event ((viewer 2d-viewer) (event sn:motion-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (center-pt
                 radius
                 ϴ
                 rotation-enabled
                 zoom
                 pan
                 sgl:view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (let ((n-scale 1/160))
          (setf center-pt (if pan
                              (v+ center-pt
                                  (vec2 (* radius (- sn:x) n-scale)
                                        (* radius (- sn:z) n-scale)))
                              center-pt)
                radius (if zoom
                           (max 1.0
                                (+ radius (* radius (+ sn:y) n-scale)))
                           radius)
                ϴ (if rotation-enabled
                      (mod (+ ϴ
                              (* sn:ry n-scale))
                           (* 4 pi))
                      ϴ)
                sgl:view-changed t))))))

(defmethod handle-3d-mouse-event ((viewer 2d-viewer) (event sn:button-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (sgl:view-changed
                 rotation-enabled
                 sgl:objects) viewer
      (cond ((sn:button-press-p event 1)
             (reset-view-safe viewer)
             (setf view-changed t)
             t)
            ((sn:button-press-p event 0)
             (setf rotation-enabled (not rotation-enabled))
             t)
            ))))
