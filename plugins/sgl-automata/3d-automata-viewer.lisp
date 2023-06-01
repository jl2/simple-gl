;; 3d-automata-viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:sgl-automata)

(defclass 3d-automata-viewer (3d-viewer)
  ((iteration :initform 0 :type fixnum :initarg :iteration)
   (animating :initform nil :type (or nil t) :initarg :animating)
   (level :initform 0 :type fixnum :initarg :level))
  (:documentation "A viewer with 3d mouse camera navigation."))


(defmethod sgl::handle-key ((viewer 3d-automata-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (sgl::with-viewer-lock (viewer)
    (with-slots (sgl:view-changed level sgl:objects iteration animating sgl:radius sgl:center-pt) viewer
      (let* ((level-delta 0)
             (iter-delta 0)
             (force-redraw nil)
             (reset nil))
        (prog1
            (cond

              ((and (eq key :n) (find action '(:repeat :press)))
               (incf iter-delta)
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :p) (find action '(:repeat :press)))
               (decf iter-delta)
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :z) (find action '(:repeat :press)))
               (setf reset t)
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)

              ((and (eq key :a) (find action '(:repeat :press)))
               (setf animating (not animating))
               (setf sgl:view-changed t)
               t)

              ((and (eq key :i) (find action '(:repeat :press)))
               (setf level-delta 1)
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)
              ((and (eq key :o) (find action '(:repeat :press)))
               (setf level-delta -1)
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)
              ((and (eq key :q) (eq action :press))
               (setf force-redraw t)
               (setf sgl:view-changed t)
               t)

              (t nil))
          (loop
            :for (name . obj) :in sgl:objects
            :do
               (with-slots ((this-animating animating) current-iteration level generated-iteration) obj

                 (when force-redraw
                   (setf generated-iteration -1))
                 (setf current-iteration (if reset
                                             0
                                             (max 0
                                                  (+ current-iteration iter-delta))))
                 (setf level (max 0 (+ level level-delta)))
                 (setf this-animating animating)
                 (sgl:set-uniform obj "view_transform" (sgl:view-matrix viewer) :mat4)))))))
  (call-next-method))



(defmethod sgl:handle-3d-mouse-event ((viewer 3d-automata-viewer) (event sn:button-event))
  (sgl:with-viewer-lock (viewer)
    (with-slots (sgl:view-changed
                 sgl:objects) viewer
      (let ((delta (cond ((sn:button-press-p event 0)
                          -1)
                         ((sn:button-press-p event 1)
                          1)
                         (t 0))))
        (when (not (zerop delta))
          (loop :for (name . obj) :in sgl:objects
                :do
                   (with-slots (current-iteration) obj
                     (setf current-iteration (max 0
                                                  (+ current-iteration delta))))))))))
