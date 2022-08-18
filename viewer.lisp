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
  #+(or windows linux freebsd) t
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defvar *viewers* (make-hash-table :test 'equal))
(declaim (inline find-viewer add-viewer rm-viewer rm-all-viewers))
(defun find-viewer (window)
  (gethash (cffi:pointer-address window) *viewers*))

(defun add-viewer (window viewer)
  (setf (gethash (cffi:pointer-address window) *viewers*) viewer))

(defun rm-viewer (window)
  (remhash (cffi:pointer-address window) *viewers*))

(defun rm-all-viewers ()
  (loop
    :for window :being :the :hash-keys :of *viewers*
      :using (hash-value viewer)
    :do
       (cleanup viewer)
       (glfw:destroy-window window))
  (setf *viewers* (make-hash-table :test 'equal)))

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
      (handle-scroll viewer window cpos x-scroll y-scroll))))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (when-let (viewer (find-viewer window))
    (handle-resize viewer window width height)))

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (format t "Error: ~a~%" message))

(defclass viewer ()
  ((window :initform nil)
   (viewer-mutex :initform (bt:make-lock "viewer-lock"))

   (objects :initform nil
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

   (last-update-time :initform 0)

   (seconds-between-updates :initform (/ 1 30)
                            :initarg :seconds-between-updates)

   (cull-face :initform nil
              :initarg :cull-face
              :accessor cull-face)
   (front-face :initform :ccw
               :initarg :front-face
               :accessor front-face)

   (blend :initform t
          :initarg :blend
          :accessor blend)

   (background-color :initform (vec4 0.08f0 0.08f0 0.08f0 1.0)
                     :initarg :background
                     :accessor background-color)

   (discarded-objects :initform nil)

   (initial-height :initform 800 :initarg :initial-height)
   (initial-width :initform 800 :initarg :initial-width)
   (previous-seconds :initform 0.0)
   (frame-count :initform 1))
  (:documentation "A collection of objects and a vieweport."))

(defmacro with-viewer-lock ((viewer) &body body)
  `(with-slots (viewer-mutex) ,viewer
     ;; (format t "Acquiring mutex ~a ~%" viewer-mutex)
     (bt:with-lock-held (viewer-mutex)
       ;; (format t "Acquired mutex ~a ~%" viewer-mutex)
       ,@body)))

(defmethod initialize ((viewer viewer) &key)
  (with-viewer-lock (viewer)
    (with-slots (last-update-time objects view-xform) viewer
      (setf last-update-time 0)
      (loop
        :for (nil . object) :in objects
        :do
           (initialize object)
           (set-uniform object "view_transform" view-xform :mat4)))))


(defmethod cleanup ((viewer viewer))
  (with-viewer-lock (viewer)
    (clean-discarded viewer)
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (cleanup object))))

(defun clean-discarded (viewer)
  (with-slots (discarded-objects) viewer
    (loop
      :for (nil . object) :in discarded-objects
      :do
         (cleanup object))
    (setf discarded-objects nil)))

(defgeneric reset-view (viewer)
  (:documentation "Reset view to its initial conditions."))


(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ;; ESC to exit
    ((and (eq key :escape) (eq action :press))
     (glfw:set-window-should-close window)
     t)

    ;; r to rebuild shaders
    ((and (eq key :r) (eq action :press))
     (format t "Rebuilding shaders...~%")
     (with-viewer-lock (viewer)
       (with-slots (objects view-changed) viewer
         (loop :for (nil . object) :in objects :do
           (ensure-initialized object)
           (rebuild-style object))
         (setf view-changed t)))
     t)

    ;; f to refill buffers
    ((and (eq key :b) (eq action :press))
     (format t "Reloading buffers ~%")
     (with-viewer-lock (viewer)
       (with-slots (blend) viewer
         (setf blend (not blend)))
       t))

    ;; i to show gl info
    ((and (eq key :i) (eq action :press))
     (show-open-gl-info)
     (show-info viewer)
     t)

    ;; s to show gl state
    ((and (eq key :s) (eq action :press))
     (with-viewer-lock (viewer)
       (show-gl-state))
     t)

    ;; f to toggle printing fps
    ((and (eq key :f) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (show-fps) viewer
         (setf show-fps (not show-fps)))
       t))

    ;; f1
    ((and (eq key :f1) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (cull-face) viewer
         (setf cull-face (if (eq cull-face :cull-face)
                             nil
                             :cull-face))
         (format t "Cull face: ~a~%" cull-face))
       t))

    ((and (eq key :f2) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (front-face) viewer
         (setf front-face (if (eq front-face :cw)
                              :ccw
                              :cw))
         (format t "Front face: ~a~%" front-face))
       t))
    ((and (eq key :f12) (eq action :press))
     ;; TODO: Run this in a background thread?
     (let* ((win-size (glfw:get-window-size))
            (width (car win-size))
            (height(cadr win-size))
            (data (gl:read-pixels 0 0 width height :rgba :unsigned-byte))
            (fname (format nil "/home/jeremiah/screenshots/sgl-screenshot~4,'0d.png" (random 10000))))
       (format t "Screenshot to: ~a~%" fname)
       (zpng:write-png
        (make-instance 'zpng:png :color-type :truecolor-alpha
                                 :image-data (make-array (array-dimensions data) :element-type '(unsigned-byte 8)
                                                                                 :initial-contents data)
                                 :width width
                                 :height height)
        fname)
       t))
    (t
     (with-viewer-lock (viewer)
       (funcall #'some #'identity
                (loop
                  :for (nil . object) :in (objects viewer)
                  :collect
                  (handle-key object window key scancode action mod-keys)))))))


(defmethod handle-resize ((viewer viewer) window width height)
  (with-viewer-lock (viewer)
    (with-slots (aspect-ratio) viewer
      (gl:viewport 0 0 width height)
      (setf aspect-ratio
            (if (< width height )
                (/ height width 1.0)
                (/ width height 1.0))))
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (handle-resize object window width height))))

(defmethod handle-click ((viewer viewer) window click-info)
  (with-viewer-lock (viewer)
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (handle-click object window click-info))))

(defmethod handle-scroll ((viewer viewer) window cpos x-scroll y-scroll)
  (with-viewer-lock (viewer)
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (handle-scroll object window cpos x-scroll y-scroll))))

(defmethod update ((viewer viewer) elapsed-seconds)
  (with-slots (objects
               view-xform
               view-changed
               camera-position
               last-update-time
               seconds-between-updates) viewer
    (flet ((update-object (obj)
             (let ((object (cdr obj)))
               (when view-changed
                 (set-uniform object "camera_position" camera-position :vec3)
                 (set-uniform object "view_transform" view-xform :mat4))
               (set-uniform object "time" elapsed-seconds :float)
               (cond
                 ((not (initialized-p object))
                  object)
                 ((> (- elapsed-seconds last-update-time) seconds-between-updates)
                  (multiple-value-list (update object elapsed-seconds)))
                 (t nil)))))
      (let ((update-results (lparallel:pmapcar #'update-object objects)))
        (loop
          :for obj :in update-results
          :when (and obj (atom obj)) :do
            (initialize obj)
            (setf last-update-time elapsed-seconds)
          :when (consp obj) :do
            (setf last-update-time elapsed-seconds)
            (dolist (buffer obj)
              (when buffer
                (reload buffer))))))
    (setf view-changed nil)))

(defmethod render ((viewer viewer))
  (with-slots (objects) viewer
    (loop
      :for (nil . object) :in objects
      :do
         (ensure-initialized object)
         (render object))))

(defgeneric display-in (object viewer)
  (:documentation "Display object in a viewer."))

(defun gl-init ()
  (glfw:initialize))

(defun gl-terminate ()
  (glfw:terminate))

(cffi:defcallback gl-debug-callback :void ((source :int) (type :int) (id :int)
                                           (severity :int) (length :int)
                                           (message :string) (param :pointer))
  (declare (ignorable param length))
  (format t "~a ~a ~a ~a~%    ~s~%"
          (cffi:foreign-enum-keyword '%gl:enum source)
          (cffi:foreign-enum-keyword '%gl:enum type)
          (cffi:foreign-enum-keyword '%gl:enum severity)
          id
          message))

(defmethod display ((viewer viewer))
  "High level function to display a viewer and start processing in a background thread."

  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  ;; (glfw:set-error-callback 'error-callback)
  (gl-init)

  (when (null lparallel:*kernel*)
    (setf lparallel:*kernel* (lparallel:make-kernel 24)))
  (tmt:with-body-in-main-thread (:blocking nil)
    (let* ((window (glfw:create-window :title "OpenGL Viewer"
                                       :width (slot-value viewer 'initial-width)
                                       :height (slot-value viewer 'initial-height)
                                       :decorated t
                                       :opengl-profile :opengl-core-profile
                                       :context-version-major 4
                                       :context-version-minor 0
                                       :opengl-debug-context t
                                       :opengl-forward-compat *want-forward-context*
                                       :samples 0
                                       :resizable t)))
      (when (null window)
        (format t "Could not create-window!")
        (error "Could not create-window!"))

      (with-viewer-lock (viewer)
        (setf (slot-value viewer 'window) window))

      (add-viewer window viewer)

      (gl:enable :debug-output-synchronous)
      (%gl:debug-message-callback (cffi:callback gl-debug-callback)
                                  (cffi:null-pointer))
      (%gl:debug-message-control :dont-care :dont-care :dont-care 0 (cffi:null-pointer) :true)

      (unwind-protect
           (handler-case
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
                            :program-point-size
                            )
                 (gl:depth-func :lequal)
                 ;; The event loop
                 (with-slots (previous-seconds show-fps desired-fps
                              blend
                              cull-face front-face background-color)
                     viewer

                   ;; Load objects for the first time
                   (initialize viewer)
                   #+spacenav(sn:sensitivity 0.125d0)
                   (loop
                     :with start-time = (glfw:get-time)
                     :for frame-count from 0
                     :until (glfw:window-should-close-p window)

                     :for current-seconds = (glfw:get-time)
                     :for elapsed-seconds = (- current-seconds previous-seconds)
                     :for elapsed-time = (- current-seconds start-time)

                     :when (and show-fps (> elapsed-seconds 0.25))
                       :do
                          (format t "~,3f fps~%" (/ frame-count elapsed-seconds))
                          (setf previous-seconds current-seconds)
                          (setf frame-count 0)

                          ;; This do is important...
                     :do (progn
                           (glfw:swap-buffers window)
                           #+spacenav
                           (when-let (ev (sn:poll-event))
                             (sn:remove-events :motion)
                             (handle-3d-mouse-event viewer ev)))
                     :do
                        (with-viewer-lock (viewer)

                          (clean-discarded viewer)

                          ;; Update for next frame
                          (update viewer elapsed-time)
                          ;; Apply viewer-wide drawing settings
                          (gl:clear-color (vx background-color)
                                          (vy background-color)
                                          (vz background-color)
                                          (vw background-color))
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


                          (render viewer))

                     :do (glfw:poll-events)
                     :do (let* ((now (glfw:get-time))
                                (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                             now)))
                           ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                           (when (> rem-time 0)
                             (sleep rem-time))))))

             (t (err)
               (format t "Caught:  ~a~%" err)
               (inspect err)))
        (progn
          ;; Cleanup before exit
          (cleanup viewer)
          (rm-viewer window))
        #+spacenav(sn:sn-close)
        (glfw:destroy-window window)
        (glfw:poll-events)))))

(defun show-gl-state ()
  "Print debug information about the OpenGL state."
  (loop
    :for field :in '(:active-texture
                     :array-buffer-binding
                     :blend
                     :current-program
                     :line-width
                     :vertex-array-binding
                     :viewport)
    :do
       (format t "~a : ~a~%" field (gl:get-integer field))))

(defun show-open-gl-info ()
  "Print OpenGL limits"
  (loop
    :for field :in '(:max-combined-texture-image-units
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
    :do
       (format t "~a : ~a~%" field (gl:get-integer field))))

(defmethod show-info ((viewer viewer) &key (indent 0))
  (with-viewer-lock (viewer)
    (let ((this-ws (indent-whitespace indent)))
      (show-slots this-ws viewer  '(objects view-xform aspect-ratio show-fps desired-fps
                                    cull-face front-face background-color
                                    window previous-seconds frame-count))
      (loop for (nil . object) in (objects viewer) :do
        (show-info object :indent (1+ indent))))))

(defgeneric add-object (viewer name object))
(defmethod add-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects) viewer
      (when (null (assoc name objects))
        (push (cons name object) objects)))))

(defgeneric rm-object (viewer name))
(defmethod rm-object (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects discarded-objects) viewer
      (when (assoc name objects)
        (push (assoc name objects) discarded-objects)
        (setf objects (remove name objects :key #'car))))))

(defgeneric replace-object (viewer name object))
(defmethod replace-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects discarded-objects) viewer
      (when (assoc name objects)
        (push (assoc name objects) discarded-objects)
        (setf objects (remove name objects :key #'car))
        (push (cons name object) objects)))))

(defun big-enough (val &optional (tol 0.0001))
  (> (abs val) tol))

(defun pause (viewer timeout)
  (sgl::with-viewer-lock (viewer)
    (sleep timeout)))
