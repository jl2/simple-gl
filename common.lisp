;; common.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(deftype point ()
  'vec3)

(deftype normal ()
  'vec3)

(deftype color ()
  'vec4)

(defgeneric initialize (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric initialize-shaders (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric initialize-buffers (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric initialize-uniforms (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric initialize-textures (object &key)
  (:documentation "Initialize an OpenGL object."))

(defgeneric render (object)
  (:documentation "Render an OpenGL object."))

(defgeneric bind (object)
  (:documentation "Bind object."))

(defgeneric reload (object)
  (:documentation "Copy new data for object to OpenGL."))

(defgeneric update (object elapsed-seconds)
  (:documentation "Called on an object *before* rendering to update for the next animation frame."))

(defgeneric cleanup (object)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

;; Input handlers
(defgeneric handle-key (object window key scancode action mod-keys)
  (:documentation "Handle a GLFW key press.  Return non-nil if handled."))

(defgeneric handle-click (object window click-info)
  (:documentation "Handle mouse move."))

(defgeneric handle-scroll (object window cpos x-scroll y-scroll)
  (:documentation "Handle scrolling."))

(defgeneric handle-resize (object window width height)
  (:documentation "Handle window resize."))

(defun show-slots (white-space object slots)
  (dolist (slot slots)
    (cond ((null slot)
           (format t "~%"))
          (t
           (format t "~a~a: ~a~%" white-space slot (slot-value object slot))))))

(defgeneric show-info (object &key indent)
  (:documentation "Show OpenGL information for an object."))



#+spacenav
(defgeneric handle-3d-mouse-event (object event)
  (:documentation "Handle a spacenav 3D mouse event."))

#+spacenav
(declaim (inline handle-3d-mouse-event))
#+spacenav
(defmethod handle-3d-mouse-event ((object t) (event t))
  (declare (ignorable object event))
  nil)
