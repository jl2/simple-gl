;; viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass mouse-click ()
  ((cursor-pos :initarg :cursor-pos)
   (button :initarg :button)
   (action :initarg :action)
   (mod-keys :initarg :mod-keys)
   (time :initarg :time))
  (:documentation "Mouse click information."))


(defparameter *want-forward-context*
  #+(or windows linux freebsd) nil
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")


(let ((viewers (make-hash-table :test 'equal)))
  ;;(declare (inline find-viewer add-viewer rm-viewer rm-all-viewers))
  (defun find-viewer (window)
    (gethash (cffi:pointer-address window) viewers))

  (defun add-viewer (window viewer)
    (setf (gethash (cffi:pointer-address window) viewers) viewer))

  (defun rm-viewer (window)
    (remhash (cffi:pointer-address window) viewers))

  (defun rm-all-viewers ()
    (loop
      for window being the hash-keys of viewers
        using (hash-value viewer)
      do
         (cleanup viewer)
         (glfw:destroy-window window))
    (setf viewers (make-hash-table :test 'equal))))

;; Keyboard callback.
(glfw:def-key-callback keyboard-handler (window key scancode action mod-keys)
  (when-let (viewer (find-viewer window))
    (handle-key viewer window key scancode action mod-keys)))

;; Mouse handler callback
(glfw:def-mouse-button-callback mouse-handler (window button action mod-keys)
  (when-let (viewer (find-viewer window))
    (let* ((cpos (glfw:get-cursor-position window))
           (click-info (make-instance 'mouse-click
                                      :cursor-pos cpos
                                      :mod-keys mod-keys
                                      :action action
                                      :button button
                                      :time (glfw:get-time))))
      (handle-click viewer window click-info))))

;; GLFW scroll handler
(glfw:def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (when-let (viewer (find-viewer window))
    (let ((cpos (glfw:get-cursor-position window)))
      (format t "scroll ~a ~a ~a ~a~%" window cpos x-scroll y-scroll)
      (handle-scroll viewer window cpos x-scroll y-scroll))))

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (when-let (viewer (find-viewer window))
    (handle-resize viewer window width height)))

(defclass viewer ()
  ((objects :initform nil
            :initarg :objects
            :type (or null cons)
            :accessor objects)
   (view-xform :initform (meye 4)
               :initarg :xform
               :type mat4
               :accessor viewport)
   (camera-position :initarg :camera-position
                    :initform (vec3 0 0 1)
                    :accessor camera-position)

   (view-changed :initform t)

   (aspect-ratio :initform 1.0
                 :initarg :aspect-ratio
                 :type real
                 :accessor aspect-ratio)

   (show-fps :initform nil
             :initarg :show-fps
             :type t
             :accessor show-fps)
   (desired-fps :initform 60
                :initarg :desired-fps
                :type fixnum
                :accessor desired-fps)
   (wire-frame :initform nil
               :initarg :wire-frame
               :type t
               :accessor wire-frame-p)
   (cull-face :initform nil
              :initarg :cull-face
              :accessor cull-face)
   (front-face :initform :ccw
               :initarg :front-face
               :accessor front-face)
   (blend :initform t
          :initarg :blend
          :accessor blend)
   (background-color :initform (vec4 0.04f0 0.04f0 0.04f0 1.0)
                     :initarg :background
                     :accessor background-color)

   (window :initform nil)
   (previous-seconds :initform 0.0)
   (frame-count :initform 1))
  (:documentation "A collection of objects and a viewport."))

(defmethod initialize ((viewer viewer) &key)
  (with-slots (objects view-xform) viewer
    (loop
      for object in objects
      do
         (initialize object)
         (set-uniform object "view_transform" view-xform :mat4))))


(defmethod cleanup ((viewer viewer))
  (loop
    for object in (objects viewer)
    do
       (cleanup object)))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer viewer) (event sn:motion-event))
  (with-slots (objects) viewer
    (dolist (object objects)
      (handle-3d-mouse-event object event))))

(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (glfw:set-window-should-close window)
     t)

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (format t "Rebuilding shaders...~%")
     (with-slots (objects view-changed) viewer
       (dolist (object objects)
         (rebuild-style object))
       (setf view-changed t))
     t)

    ;; f to refill buffers
    ((and (eq key :b) (eq action :press))
     (format t "Reloading buffers ~%")
     (with-slots (blend) viewer
       (setf blend (not blend))
       t))

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info)
     (show-info viewer)
     t)

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (show-gl-state)
     t)

    ;; f to toggle printing fps
    ((and (eq key :f) (eq action :press))
     (with-slots (show-fps) viewer
       (setf show-fps (not show-fps))
       t))

    ;; w to toggle wireframe
    ((and (eq key :w) (eq action :press))
     (with-slots (wire-frame) viewer
       (setf wire-frame (if wire-frame nil t))
       (format t "Wire-frame ~a~%" wire-frame)
       t))

    ;; f1
    ((and (eq key :f1) (eq action :press))
     (with-slots (cull-face) viewer
       (setf cull-face (if (eq cull-face :cull-face)
                           nil
                           :cull-face))
       (format t "Cull face: ~a~%" cull-face)
       t))

    ((and (eq key :f2) (eq action :press))
     (with-slots (front-face) viewer
       (setf front-face (if (eq front-face :cw)
                            :ccw
                            :cw))
       (format t "Front face: ~a~%" front-face)
       t))
    ((and (eq key :f3) (eq action :press))
     (let* ((win-size (glfw:get-window-size))
            (width (car win-size))
            (height(cadr win-size))
            (data (gl:read-pixels 0 0 width height :rgba :unsigned-byte)))
       (zpng:write-png (make-instance 'zpng:png :color-type :truecolor-alpha
                                                :image-data (make-array (array-dimensions data) :element-type '(unsigned-byte 8)
                                                                        :initial-contents data)
                                                :width width
                                                :height height)
                       "/home/jeremiah/screenshots/sgl-screenshot.png")
     t))
    (t
     (funcall #'some #'identity
              (loop for object in (objects viewer)
                    collect (handle-key object window key scancode action mod-keys))))))


(defmethod handle-resize ((viewer viewer) window width height)
  (with-slots (aspect-ratio) viewer
    (gl:viewport 0 0 width height)
    (setf aspect-ratio
          (if (< width height )
              (/ height width 1.0)
              (/ width height 1.0))))
  (loop
    for object in (objects viewer)
    do
       (handle-resize object window width height)))

(defmethod handle-click ((viewer viewer) window click-info)
  (loop
    for object in (objects viewer)
    do
       (handle-click object window click-info)))

(defmethod handle-scroll ((viewer viewer) window cpos x-scroll y-scroll)
  (loop
    for object in (objects viewer)
    do
       (handle-scroll object window cpos x-scroll y-scroll)))



(defmethod update ((viewer viewer) elapsed-seconds)

  (with-slots (objects view-xform view-changed camera-position) viewer
    (loop
      for object in objects
      when view-changed do
        (set-uniform object "camera_position" camera-position :vec3)
        (set-uniform object "view_transform" view-xform :mat4)
      do
         (update object elapsed-seconds))
    (setf view-changed nil)))

(defmethod render ((viewer viewer))
  (with-slots (objects) viewer
    (loop
      for object in objects
      do
         (render object))))

(defgeneric display-in (object viewer)
  (:documentation "Display object in a viewer."))

(defun gl-init ()
  (glfw:initialize))

(defun gl-terminate ()
  (glfw:terminate))


(defmethod display-in ((object t) (viewer viewer))
  "High level function to display an object or viewer."

  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  ;; (glfw:set-error-callback 'error-callback)
  (gl-init)

  (with-slots (objects) viewer
    (setf objects (ensure-list object)))

  (let* ((window (glfw:create-window :title "OpenGL Viewer"
                                     :width 1000
                                     :height 1000
                                     :decorated nil
                                     :opengl-profile :opengl-core-profile
                                     :context-version-major 4
                                     :context-version-minor 0
                                     :opengl-forward-compat *want-forward-context*
                                     :samples 0
                                     :resizable t)))
    (when (null window)
      (format t "Could not create-window!")
      (error "Could not create-window!"))

    (unwind-protect
         (progn
           #+spacenav(sn:sn-open)
           ;; GLFW Initialization
           (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address)

           (add-viewer window viewer)

           (glfw:set-key-callback 'keyboard-handler window)
           (glfw:set-mouse-button-callback 'mouse-handler window)
           (glfw:set-scroll-callback 'scroll-handler window)
           (glfw:set-framebuffer-size-callback 'resize-handler window)

           ;; Initialize OpenGL state
           (gl:enable :line-smooth
                      :polygon-smooth
                      :depth-test
                      )
           (gl:depth-func :lequal)

           ;; The event loop
           (with-slots (previous-seconds show-fps desired-fps
                        blend
                        cull-face front-face wire-frame background-color)
               viewer

             (gl:clear-color (vx background-color)
                             (vy background-color)
                             (vz background-color)
                             (vw background-color))

             ;; Load objects for the first time
             (initialize viewer)
             #+spacenav(sn:sensitivity 0.0125d0)
             (loop
               with start-time = (glfw:get-time)
               for frame-count from 0
               until (glfw:window-should-close-p window)

               for current-seconds = (glfw:get-time)
               for elapsed-seconds = (- current-seconds previous-seconds)
               for elapsed-time = (- current-seconds start-time)

               when (and show-fps (> elapsed-seconds 0.25))
                 do
                    (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                    (setf previous-seconds current-seconds)
                    (setf frame-count 0)

                    ;; This do is important...
               do (progn
                    (glfw:swap-buffers window)
                    #+spacenav
                    (when-let (ev (sn:poll-event))
                      (sn:remove-events :motion)
                      (handle-3d-mouse-event viewer ev)))
               do
                  ;; Update for next frame
                  (update viewer elapsed-time)
                  ;; Apply viewer-wide drawing settings
                  (gl:clear :color-buffer :depth-buffer)

                  (if cull-face
                      (gl:enable :cull-face)
                      (gl:disable :cull-face))

                  (cond (blend
                         (gl:enable :blend)
                         (gl:blend-func :src-alpha
                                        :one-minus-src-alpha))
                        (t (gl:disable :blend)))
                  (gl:front-face front-face)

                  (gl:polygon-mode :front-and-back
                                   (if wire-frame
                                       :line
                                       :fill))


                  (render viewer)

               do (glfw:poll-events)
               do (let* ((now (glfw:get-time))
                         (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                      now)))
                    ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                    (when (> rem-time 0)
                      (sleep rem-time))))))

      (progn
        ;; Cleanup before exit
        (cleanup viewer)
        (rm-viewer window))
      #+spacenav(sn:sn-close)
      (glfw:destroy-window window)
      (glfw:poll-events)))
  (gl-terminate))

(defun show-gl-state ()
  "Print debug information about the OpenGL state."
  (loop
    for field in '(:active-texture
                   :array-buffer-binding
                   :blend
                   :current-program
                   :line-width
                   :vertex-array-binding
                   :viewport)
    do
       (format t "~a : ~a~%" field (gl:get-integer field))))

(defun show-open-gl-info ()
  "Print OpenGL limits"
  (loop
    for field in '(:max-combined-texture-image-units
                   :max-cube-map-texture-size
                   :max-draw-buffers
                   :max-fragment-uniform-components
                   :max-texture-size
                   ;; :max-varying-floats
                   :max-vertex-attribs
                   :max-vertex-texture-image-units
                   :max-vertex-uniform-components
                   :max-viewport-dims
                   :texture-binding-2d
                   :max-patch-vertices
                   :stereo)
    do
       (format t "~a : ~a~%" field (gl:get-integer field))))

(defmethod show-info ((viewer viewer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent)))
    (show-slots this-ws viewer  '(objects view-xform aspect-ratio show-fps desired-fps
                                  wire-frame cull-face front-face background-color
                                  window previous-seconds frame-count))
    (with-slots (objects) viewer
      (dolist (object objects)
        (show-info object :indent (1+ indent))))))

(defgeneric add-object (viewer object))
(defmethod add-object (viewer object)
  (with-slots (objects) viewer
      (push object objects)))


(defun view-matrix (radius theta gamma)
  (let*  ((tval (* radius (cos theta)))
          (yv (* radius (sin theta)))
          (xv (* tval (cos gamma)))
          (zv (* tval (sin gamma))))
  (m* (mperspective 60.0 1.0 0.1 1000.0)
      (mlookat (vec3 xv
                     yv
                     zv)
               (vec3 0 0 0)
               (vec3 0 1 0)))))

(defclass 3d-mouse-nav-viewer (viewer)
  ((radius :initform 1.0)
   (theta :initform 0.0)
   (gamma :initform 0.0)
   (view-xform :initform (view-matrix 1.0 0.0 0.0)
               :type mat4))
  (:documentation "A viewer with 3d mouse camera navigation."))

(defun reset-view (viewer)
  (with-slots (view-changed objects aspect-ratio radius theta gamma view-xform) viewer
    (setf radius pi)
    (setf theta (/ pi 4))
    (setf gamma (/ pi 6))
    (setf view-xform (view-matrix radius theta gamma))
    (loop
      for object in objects
      do
         (set-uniform object "view_transform" view-xform :mat4))
    (setf view-changed t)))

(defmethod handle-key ((viewer 3d-mouse-nav-viewer) window key scancode action mod-keys)
  (declare (ignorable window scancode mod-keys))
  (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
    (let* ((multiplier (if (find :shift mod-keys) 8 1))
           (angle-inc (* multiplier (/ pi 180)))
           (linear-inc (* multiplier 0.5)))

      (cond

        ((and (eq key :f5) (eq action :press))
         (reset-view viewer)
         (setf view-changed t)
         t)


        ((and (eq key :left) (find action '(:repeat :press)))
         (setf gamma
               (+ gamma angle-inc))
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

(defun big-enough (val &optional (tol 0.0001))
  (> (abs val) tol))

#+spacenav
(defmethod handle-3d-mouse-event ((viewer 3d-mouse-nav-viewer) (event sn:motion-event))
  (with-slots (aspect-ratio view-xform radius theta gamma view-changed) viewer
    (with-slots (sn:x sn:y sn:z  sn:rx sn:ry sn:rz) event
      (let* ((linear-scale (/ 1.0 400))
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
                          (min 1000.0
                               (+ radius linear-inc))))
        (setf view-xform (view-matrix radius theta gamma))
        (setf view-changed t)))))
