;; viewer.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defparameter *want-forward-context*
  #+(or windows linux freebsd) t
  #+darwin t
  "Whether or not to ask for a 'forward compatible' OpenGL context.  Required for OSX.")

(defparameter *error-stream* t
  "Stream to write error information (usually GLFW errors).")

(defvar  *display-in-main-thread* t
  "t if GLFW windows will run in the main thread.")

(defvar *viewers* (make-hash-table :test 'equal)
  "All viewers that have been created.")

(declaim (inline find-viewer add-viewer rm-viewer rm-all-viewers))

(defun find-viewer (window)
  "Find a viewer associated with the given GLFW window."
  (gethash (cffi:pointer-address window) *viewers*))

(defun add-viewer (window viewer)
  "Add a new viewer for the GLFW window."
  (setf (gethash (cffi:pointer-address window) *viewers*) viewer))

(defun rm-viewer (window)
  "Remove the viewer associated with the GLFW viewer."
  (remhash (cffi:pointer-address window) *viewers*))

(defun rm-all-viewers ()
  "Remove all viewers."
  (loop
    :for window :being :the :hash-keys :of *viewers*
      :using (hash-value viewer)
    :do
       (cleanup viewer)
       (glfw:destroy-window window))
  (setf *viewers* (make-hash-table :test 'equal)))

;; Keyboard callback.
(glfw:def-key-callback keyboard-handler (window key scancode action mod-keys)
  (progn
    "GLFW keyboard handler.  Finds a viewer for the window and calls the (handle-key) method."
    (when-let (viewer (find-viewer window))
      (handle-key viewer window key scancode action mod-keys))))

;; Mouse handler callback
(glfw:def-mouse-button-callback mouse-handler (window button action mod-keys)
  (progn
    "GLFW mouse handler.  Finds a viewer for the window and calls the (handle-click) method."
    (when-let (viewer (find-viewer window))
      (let* ((cpos (glfw:get-cursor-position window))
             (click-info (make-instance 'mouse-click
                                        :cursor-pos cpos
                                        :mod-keys mod-keys
                                        :action action
                                        :button button
                                        :time (glfw:get-time))))
        (handle-click viewer window click-info)))))

;; GLFW scroll handler
(glfw:def-scroll-callback scroll-handler (window x-scroll y-scroll)
  (progn
    "GLFW scroll handler.  Finds a viewer for the window and calls the (handle-scroll) method."
    (when-let (viewer (find-viewer window))
      (let ((cpos (glfw:get-cursor-position window)))
        (handle-scroll viewer window cpos x-scroll y-scroll)))))

;; Resize event handler
(glfw:def-framebuffer-size-callback resize-handler (window width height)
  (progn
    "GLFW resize handler.  Finds a viewer for the window and calls the (handle-resize) method."
    (when-let (viewer (find-viewer window))
      (handle-resize viewer window width height))))

;; GLFW error callback
(glfw:def-error-callback error-callback (message)
  (progn
    "GLFW error callback.  Writes errors to *error-stream*"
    (format *error-stream* "Error: ~a~%" message)))


(defmacro with-viewer-lock ((viewer) &body body)
  `(with-slots (viewer-mutex) ,viewer
     (bt:with-lock-held (viewer-mutex)
       ,@body)))

(defmethod initialize ((viewer viewer) &key)
  (with-viewer-lock (viewer)
    (with-slots (last-update-time objects view-changed) viewer
      (setf last-update-time 0)
      (loop
        :for (nil . object) :in objects
        :do
           (initialize object)
           (set-uniform object "view_transform" (view-matrix viewer) :mat4))
      (setf view-changed t))))


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

(defun reset-view (viewer)
  (with-viewer-lock (viewer)
    (reset-view-safe viewer)))

(defun rebuild-buffers (viewer)
  (format t "Refilling buffers~%")
  (with-viewer-lock (viewer)
    (with-slots (objects view-changed) viewer
      (loop
        :for (nil . object) :in objects
        :do
           (ensure-initialized object)
           (reload object))
      (setf view-changed t))))

(defun rebuild-shaders (viewer)
  (format t "Rebuilding shaders...~%")
  (with-viewer-lock (viewer)
    (with-slots (objects view-changed) viewer
      (loop
        :for (nil . object) :in objects
        :do
           (ensure-initialized object)
           (rebuild-style object))
      (setf view-changed t))))

(defun rebuild-textures (viewer)
  (format t "Reloading textures...~%")
  (with-viewer-lock (viewer)
    (with-slots (objects view-changed) viewer
      (loop :for (nil . object) :in objects :do
        (ensure-initialized object)
        (refill-textures object))
      (setf view-changed t))))

(defmethod handle-key ((viewer viewer) window key scancode action mod-keys)
  (cond
    ((and (eq key :escape) (eq action :press))
     (glfw:set-window-should-close window)
     t)

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

    ((and (eq key :f3) (eq action :press))
     (rebuild-shaders viewer)
     t)

    ((and (eq key :f4) (eq action :press))
     (rebuild-textures viewer)
     t)

    ((and (eq key :f5) (eq action :press))
     (rebuild-buffers viewer)
     t)


    ((and (eq key :f6) (eq action :press))
     (format t "Toggling blending ~%")
     (with-viewer-lock (viewer)
       (with-slots (blend) viewer
         (setf blend (not blend)))
       t))

    ((and (eq key :f7) (eq action :press))
     (show-open-gl-info)
     (show-info viewer)
     t)

    ((and (eq key :f8) (eq action :press))
     (with-viewer-lock (viewer)
       (show-gl-state))
     t)

    ((and (eq key :f9) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (show-fps) viewer
         (setf show-fps (not show-fps)))
       t))

    ((and (eq key :f10) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (enable-update) viewer
         (cond ((integerp enable-update)
                (setf enable-update t))
               ((null enable-update)
                (setf enable-update t))
               (t
                (setf enable-update nil)))
         (format t "enable-update: ~a~%" enable-update))
       t))

    ((and (eq key :f11) (eq action :press))
     (with-viewer-lock (viewer)
       (with-slots (enable-update) viewer
         (cond ((null enable-update)
                (setf enable-update 1))
               ((integerp enable-update)
                (incf enable-update)))
         (format t "enable-update: ~a~%" enable-update))
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
               view-changed
               enable-update
               camera-position
               last-update-time
               seconds-between-updates)
      viewer

    ;; This is a little convoluted because the update is done by a pool of
    ;; worker threads, but may trigger OpenGL state changes that need to be
    ;; handled in the main thread.
    ;; update-object runs in a thread pool for each object
    ;; It returns the object if the object needs to be initialized
    ;; Otherwise it returns a list of buffers that need to be reloaded
    ;; by OpenGL.  As far as I know, there's no way to update buffers off of
    ;; the main thread.
    (let ((was-view-changed view-changed))
      ;; Change this back immediately
      (setf view-changed nil)
      (flet
          ;; Call update on objects in a worker thread and return
          ;; objects that need to be initialized in the main thread
          ;; or buffers that need to be reloaded, also in the main thread
          ((update-object (obj)
             ;; Update view info if needed, and set time shader variable
             (let ((object (cdr obj)))
               (when was-view-changed
                 (set-uniform object "camera_position" camera-position :vec3)
                 (set-uniform object "view_transform" (view-matrix viewer) :mat4))
               (set-uniform object "time" elapsed-seconds :float)
               ;; If an object isn't initialized return it so that it will be initialized in the main thread
               (cond
                 ((not (initialized-p object))
                  object)
                 ((and enable-update
                       (> (- elapsed-seconds last-update-time)
                          seconds-between-updates))
                  (update object elapsed-seconds))
                 (t nil)))))
        ;; Call update-object on each object
        (let ((update-results (lparallel:pmapcar #'update-object objects)))
          (if (integerp enable-update)
              (if (zerop (- enable-update 1))
                  (setf enable-update nil)
                  (decf enable-update)))
          (loop
            :for obj :in update-results

            ;; Initialize objects that need it
            :when (and obj (atom obj))
              :do
                 (initialize obj)
                 (setf last-update-time elapsed-seconds)

            ;; reload buffers that need it
            :when (consp obj)
              :do
                 (setf last-update-time elapsed-seconds)
                 (loop
                   :for thing :in obj
                   :when thing
                     :do
                        ;;(bind object)
                        (reload thing))))))))

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

(defun glfw-initialization ()
  )

(defmethod display ((viewer viewer))
  "High level function to display a viewer and start processing in a background thread."

  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  ;; (glfw:set-error-callback 'error-callback)
  (gl-init)

  (when (null lparallel:*kernel*)
    (setf lparallel:*kernel* (lparallel:make-kernel 24)))

  (flet
      ((window-main ()
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
                                 :program-point-size)

                      (gl:depth-func :lequal)

                      ;; The event loop
                      (with-slots (previous-seconds show-fps desired-fps
                                   blend
                                   cull-face front-face background-color)
                          viewer

                        ;; Load objects for the first time
                        (initialize viewer)

                        #+spacenav(sn:sn-open)
                        ;;#+spacenav(sn:sensitivity 0.5d0)

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


                          :do ;; This do is important...
                              (glfw:swap-buffers window)

                          #+spacenav
                           (when-let (ev (sn:poll-event))
                             (handle-3d-mouse-event viewer ev)
                             (sn:remove-events :motion))

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


                               (if cull-face
                                   (gl:enable :cull-face)
                                   (gl:disable :cull-face))

                               (cond (blend
                                      (gl:enable :blend)
                                      (gl:blend-func :src-alpha
                                                     :one-minus-src-alpha))
                                     (t (gl:disable :blend)))
                               (gl:front-face front-face)

                               (gl:clear :color-buffer
                                         :depth-buffer)
                               (render viewer))

                          :do
                             (glfw:poll-events)
                          :do
                             (let* ((now (glfw:get-time))
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

             #+spacenav (sn:sn-close)

             (glfw:destroy-window window)
             (glfw:poll-events)))))

    (cond
      (*display-in-main-thread*
       (tmt:with-body-in-main-thread (:blocking nil)
         (window-main)))
      (t (window-main)))))

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
  (format t "gl-version: ~a~%" (gl:gl-version))
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
      (show-slots this-ws viewer  '(objects aspect-ratio show-fps desired-fps
                                    cull-face front-face background-color
                                    window previous-seconds))
      (loop for (nil . object) in (objects viewer) :do
        (show-info object :indent (1+ indent))))))

(defmethod add-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects view-changed camera-position) viewer
      (when (null (assoc name objects))
        (set-uniform object "camera_position" camera-position :vec3)
        (set-uniform object "view_transform" (view-matrix viewer) :mat4))
        (push (cons name object) objects)
        (setf view-changed t))))

(defmethod rm-object (viewer name)
  (with-viewer-lock (viewer)
    (with-slots (objects discarded-objects view-changed) viewer
      (when (assoc name objects)
        (push (assoc name objects) discarded-objects)
        (setf objects (remove name objects :key #'car))
        (setf view-changed t)))))


(defmethod replace-object (viewer name object)
  (with-viewer-lock (viewer)
    (with-slots (objects discarded-objects view-changed) viewer
      (when (assoc name objects)
        (push (assoc name objects) discarded-objects)
        (setf objects (remove name objects :key #'car))
        (push (cons name object) objects)
        (setf view-changed t)))))

(defun big-enough (val &optional (tol 0.0001))
  (> (abs val) tol))

(defun pause (viewer timeout)
  (sgl::with-viewer-lock (viewer)
    (sleep timeout)))

(defmethod view-matrix ((viewer viewer))
  (meye 4))
