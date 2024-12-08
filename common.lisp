;; common.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(deftype point () 'vec3)

(deftype normal () 'vec3)

(deftype color () 'vec4)

(defun deg2rad (deg)
  (* deg
     (/ pi
        180)))



(defun rad2deg (rad)
  (/ rad
     (/ pi 180))
  )





(defgeneric use-uniform (uniform program)
  (:documentation "Pass the uniform's value into the OpenGL program."))

(defgeneric get-value (uniform)
  (:documentation "Set a generic's value and optionally type."))

(defgeneric set-value (uniform new-value &optional new-type)
  (:documentation "Set a generic's value and optionally type."))



(defgeneric reset-view-safe (viewer)
  (:documentation "Reset view to its initial conditions."))

(defgeneric use-style (style)
  (:documentation "Apply style settings."))

(defgeneric rebuild-style (object)
  (:documentation "Rebuild a style."))

(defgeneric needs-rebuild (object)
  (:documentation "Returns non-nil, usually a list of things that need updating."))

(defgeneric fill-texture (obj))

(defgeneric initialize (object &key)
  (:documentation "Initialize an OpenGL object.  Default implementation calls the initialize-* methods."))

(defgeneric initialize-shaders (object &key)
  (:documentation "Initialize the shaders associated with an OpenGL object."))

(defgeneric initialize-buffers (object &key)
  (:documentation "Initialize the buffers associated with an OpenGL object."))

(defgeneric initialize-uniforms (object &key)
  (:documentation "Initialize the uniforms associated with an OpenGL object."))

(defgeneric initialize-textures (object &key)
  (:documentation "Initialize the textures associated with object."))

(defgeneric render (object)
  (:documentation "Render an OpenGL object."))

(defgeneric bind (object)
  (:documentation "Bind buffers, textures, etc. for an object."))

(defgeneric reload (object)
  (:documentation "Copy new data for object to OpenGL."))

(defgeneric update (object elapsed-seconds)
  (:documentation "Called on an object before rendering, to update for the next animation frame.  Returns nil or an update-result"))

(defclass update-result ()
  ()
  (:documentation "The result of an update.  After an update (perform-main-thread-update update-result) is called in the OpenGL thread."))

(defgeneric perform-main-thread-update (update-result)
  (:documentation "Run the part of the update that needs to run in the OpenGL thread."))

(defclass style-rebuilder (update-result)
  ((styles :initarg :styles))
  (:documentation "An update-result who's GL thread action is to rebuild the object's shaders."))

(defclass buffer-reloader (update-result)
  ((buffers :initarg :buffers))
  (:documentation "An update-result who's GL thread action is to reload the object."))

(defclass obj-initializer (update-result)
  ((obj :initarg :obj))
  (:documentation "An update-result who's GL thread action is to initialize the object."))

(defmethod perform-main-thread-update ((result buffer-reloader))
  (with-slots (buffers) result
    (dolist (buf (ensure-list buffers))
      (reload buf))))

(defmethod perform-main-thread-update ((result style-rebuilder))
  (with-slots (styles) result
    (dolist (sty styles)
      (rebuild-style sty))))

(defmethod perform-main-thread-update ((result obj-initializer))
  (with-slots (obj) result
    (initialize obj)))


(defgeneric cleanup (object)
  (:documentation "Cleanup any OpenGL resources owned by obj."))

(defgeneric initialized-p (object)
  (:documentation "Returns nil if object is not initialized, non-nil otherwise."))

(defgeneric build-style (object)
  (:documentation "Bind the correct VAO and build object's shader programs."))

(defgeneric clone (object)
  (:documentation "Create an identical but different object."))

(defun show-slots (white-space object slots)
  "Print the specified slots of object, one per line, indenting with white-space on each line.~
   Use 'nil' slot name to print an empty line to separate values."
  (dolist (slot slots)
    (cond ((null slot)
           (format t "~%"))
          (t
           (format t "~a~a: ~a~%" white-space slot (slot-value object slot))))))

(defgeneric show-info (object &key indent)
  (:documentation "Show OpenGL information for an object."))

(defun indent-whitespace (n)
  (make-string (* 2 n) :initial-element #\space))

#+spacenav
(defgeneric handle-3d-mouse-event (object event)
  (:documentation "Handle a spacenav 3D mouse event."))

#+spacenav
(defmethod handle-3d-mouse-event ((object t) (event t))
  (declare (ignorable object event))
  nil)
