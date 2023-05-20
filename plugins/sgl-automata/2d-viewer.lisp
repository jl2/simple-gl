;; 2d-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:sgl-automata)

(defclass 2d-automata-viewer (viewer)
  ((center-pt :initform (vec2 0 0) :type vec2 :initarg :center-pt)
   (radius :initform 100 :type number :initarg :radius)
   (iteration :initform 0 :type fixnum :initarg :iteration)
   (animating :initform nil :type (or nil t) :initarg :animating)
   (level :initform 0 :type fixnum :initarg :level))
  (:documentation "A viewer with 3d mouse camera navigation."))


(defmethod sgl::view-matrix ((viewer 2d-automata-viewer))
  (with-slots (center-pt radius) viewer
    (mortho (- (vx center-pt) radius)
            (+ (vx center-pt) radius)
            (- (vy center-pt) radius)
            (+ (vy center-pt) radius)
            -1 1)))

(defmethod sgl::reset-view-safe ((viewer 2d-automata-viewer))
  (with-slots (sgl:view-changed center-pt radius) viewer
    (setf center-pt (vec2 0 0))
    (setf radius 100)))

(defmethod sgl::handle-key ((viewer 2d-automata-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (sgl::with-viewer-lock (viewer)
    (with-slots (center-pt radius sgl:view-changed level sgl:objects iteration animating) viewer
      (let* ((multiplier (if (find :shift mod-keys) 4 1.0))
             (offset (/ (* multiplier radius)
                        32.0))
             (level-delta 0)
             (iter-delta 0)
             (force-redraw nil)
             (reset nil))
        (prog1
            (cond

              ((and (eq key :n) (find action '(:repeat :press)))
               (incf iter-delta)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :p) (find action '(:repeat :press)))
               (decf iter-delta)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :r) (find action '(:repeat :press)))
               (setf reset t)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :a) (find action '(:repeat :press)))
               (setf animating (not animating))
               (setf sgl:view-changed t)
               t)
              ((and (eq key :i) (find action '(:repeat :press)))
               (setf level-delta 1)
               (setf sgl:view-changed t)
               t)
              ((and (eq key :o) (find action '(:repeat :press)))
               (setf level-delta -1)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :t) (eq action :press))
               (setf center-pt (vec2 0 0)
                     radius 100)
               (setf sgl:view-changed t)
               t)
              ((and (eq key :q) (eq action :press))
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :left) (find action '(:repeat :press)))
               (incf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :right) (find action '(:repeat :press)))
               (decf (vx center-pt) offset)
               (setf sgl:view-changed t)
               t)


              ((and (eq key :up) (find action '(:repeat :press)))
               (decf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :down) (find action '(:repeat :press)))
               (incf (vy center-pt) offset)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :page-down) (find action '(:repeat :press)))
               (setf radius (+ multiplier radius))
               (setf sgl:view-changed t)
               t)

              ((and (eq key :page-up) (find action '(:repeat :press)))
               (setf radius (max 1
                                 (- radius multiplier)))
               (setf sgl:view-changed t)
               t)

              (t nil))
          (loop
            :with offset = (vec2 radius radius)
            :with new-min-pt = (v- center-pt offset)
            :with new-max-pt = (v+ center-pt offset)
            :for (name . obj) :in sgl:objects

            :do
               (with-slots (min-pt max-pt (this-animating animating) current-iteration level generated-iteration) obj

                 (when force-redraw
                   (setf generated-iteration -1))
                 (setf current-iteration (if reset
                                             0
                                             (max 0
                                                  (+ current-iteration iter-delta))))
                 (setf level (max 0 (+ level level-delta)))
                 (setf this-animating animating)
                 (setf min-pt new-min-pt)
                 (setf max-pt new-max-pt)
                 (sgl:set-uniform obj "view_transform" (sgl:view-matrix viewer) :mat4)))))))
  (call-next-method))


#+spacenav
(defmethod sgl:handle-3d-mouse-event ((viewer 2d-automata-viewer) (event sn:motion-event))
  (format t "Caught event: ~a~%" event)
  (sgl:with-viewer-lock (viewer)
    (with-slots (center-pt
                 radius
                 level
                 sgl:view-changed) viewer
      (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
        (setf center-pt (v+ center-pt (vec2 (/  sn:x 100.0) (/ sn:z 100.0))))
        (setf radius (+ radius (/ sn:y 100.0)))
        (setf sgl:view-changed t)))))
