;; offscreen-viewer.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass offscreen-viewer (viewer)
  ((fbo :initform nil)
   (should-close :initform nil)
   (output-directory :initform "/tmp/simple-gl-offscreen-render/"
                     :initarg :output-directory))
  (:documentation "A collection of objects and a viewport that renders PNG images to a directory."))

(defmethod render ((viewer offscreen-viewer))
  (with-slots (initial-height initial-width output-directory objects) viewer
    (loop
      :for object :in objects
      :do
         (render object))
    (let* ((data (gl:read-pixels 0 0 initial-width initial-height :rgba :unsigned-byte))
           (fname (format nil "~a/sgl-screenshot~4,'0d.png" output-directory (random 10000))))
      (format t "Writing screenshot to: ~a~%" fname)
      (zpng:write-png (make-instance 'zpng:png :color-type :truecolor-alpha
                                               :image-data data
                                               ;; (make-array (array-dimensions data) :element-type '(unsigned-byte 8)
                                               ;;                                     :initial-contents data)
                                               :width initial-width
                                               :height initial-height)
                      fname)
      t)))

(defmethod cleanup ((viewer offscreen-viewer))
  (call-next-method)
  (with-slots (fbo) viewer
    (gl:delete-framebuffers (list fbo))))

(defmethod display-in ((object t) (viewer offscreen-viewer))
  "High level function to display an object or viewer."

  ;; "GLFW Event Loop function that initializes GLFW and OpenGL, creates a window,
  ;;  and runs an event loop."
  ;; (glfw:set-error-callback 'error-callback)
  (gl-init)
  (with-slots (fbo window objects should-close) viewer
    (setf fbo (gl:gen-framebuffer))
    (setf window (cffi:null-pointer))
    (setf objects (ensure-list object))
    (gl:enable :debug-output-synchronous)
    (%gl:debug-message-callback (cffi:callback gl-debug-callback)
                                (cffi:null-pointer))
    (%gl:debug-message-control :dont-care :dont-care :dont-care 0 (cffi:null-pointer) :true)
    (unwind-protect
         (handler-case
             (progn
               #+spacenav(sn:sn-open)

               (add-viewer window viewer)

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

                 (gl:clear-color (vx background-color)
                                 (vy background-color)
                                 (vz background-color)
                                 (vw background-color))

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
                      (render viewer)

                   :do (glfw:poll-events)
                   :do (let* ((now (glfw:get-time))
                              (rem-time (- (+ current-seconds (/ 1.0 desired-fps))
                                           now)))
                         ;; (format t "Start: ~a now ~a sleep ~a~%" current-seconds Now rem-time)
                         (when (> rem-time 0)
                           (sleep rem-time))))))
           (t (err)
             (format t "Caught ~a~%" err)))
      (progn
        ;; Cleanup before exit
        (cleanup viewer)
        (rm-viewer window))
      #+spacenav(sn:sn-close))
    (gl-terminate)))


(defmethod show-info ((viewer offscreen-viewer) &key (indent 0))
  (let ((this-ws (indent-whitespace indent)))
    (show-slots this-ws viewer  '(objects view-xform aspect-ratio show-fps desired-fps
                                  cull-face front-face background-color
                                  window previous-seconds frame-count))
    (with-slots (objects) viewer
      (dolist (object objects)
        (show-info object :indent (1+ indent))))))

