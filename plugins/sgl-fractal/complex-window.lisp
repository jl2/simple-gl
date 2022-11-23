;; complex-window.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:sgl-fractal)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-fractal "shaders/") sgl:*shader-dirs*))

(defclass complex-fractal-viewer (viewer)
  ())

#+spacenav
(defmethod sgl:handle-3d-mouse-event ((viewer complex-fractal-viewer) (event sn:motion-event))
  (with-slots (sgl:objects) viewer
    (loop
      :for (nil . object) :in (objects viewer)
      :do
         (handle-3d-mouse-event object event))))


(defclass complex-window (opengl-object)
  ((center :initarg :center :initform #C(0.0f0 0.0f0))
   (radius :initarg :radius :initform #C(4.0f0 4.0f0))
   (max-iterations :initarg :max-iterations :initform 100))
  (:documentation "A rectangular region in the complex plain."))


(defun compute-min-max (window)
  "Returns (values real-min real-max imag-min imag-max)"
  (with-slots (center radius) window
    (let ((cr (realpart center))
          (ci (imagpart center))
          (rr (realpart radius))
          (ri (imagpart radius)))
      (values (- cr rr)
              (+ cr rr)
              (- ci ri)
              (+ ci ri)))))

(defun update-bounds (object)
  (with-slots (buffers) object
    (let ((gl-array (slot-value (assoc-value buffers :vertices)
                                'sgl:pointer)))
      (multiple-value-bind (real-min real-max imag-min imag-max) (compute-min-max object)
        (gl-fset gl-array 3 real-min)
        (gl-fset gl-array 4 imag-max)
        (gl-fset gl-array 8 real-min)
        (gl-fset gl-array 9 imag-min)
        (gl-fset gl-array 13 real-max)
        (gl-fset gl-array 14 imag-max)
        (gl-fset gl-array 18 real-max)
        (gl-fset gl-array 19 imag-min)))
    (reload (assoc-value sgl:buffers :vertices))))

(defmethod sgl:initialize-buffers ((object complex-window) &key)
  (multiple-value-bind (real-min real-max imag-min imag-max) (compute-min-max object)
    (set-buffer object
                :vertices
                (constant-attribute-buffer
                 (list -1.0f0  1.0f0 0.0f0
                       real-min imag-max

                       -1.0f0 -1.0f0 0.0f0
                       real-min imag-min

                       1.0f0  1.0f0 0.0f0
                       real-max imag-max

                       1.0f0 -1.0f0 0.0f0
                       real-max imag-min)
                 20
                 '(("in_position" . :vec3) ("in_uv" . :vec2))
                 :free nil)))

  (set-buffer object
              :indices
              (make-instance 'sgl:index-buffer
                             :idx-count 6
                             :pointer (sgl:to-gl-array :unsigned-int 6 #(0 1 2 1 3 2))
                             :free nil)))

(defmethod sgl:initialize-uniforms ((object complex-window) &key)
  (with-slots (max-iterations) object
    (set-uniform object "maxIterations" max-iterations :int)))

(defun window-from-center-radius (center radius)
  (make-instance 'complex-window
                 :center center
                 :radius radius))

(defun window-from-min-max (&key real-min real-max imag-min imag-max)
  (let ((real-center (+ real-min (/ (- real-max real-min) 2)))
        (imag-center (+ imag-min (/ (- imag-max imag-min) 2)))
        (real-radius (/ (- real-max real-min) 2))
        (imag-radius (/ (- imag-max imag-min) 2)))
    (make-instance 'complex-window
                   :radius (complex real-radius imag-radius)
                   :center (complex real-center imag-center))))



(defun window-from-vertices (array)
  (window-from-min-max :real-min (aref array 3)
                       :real-max (aref array 13)
                       :imag-min (aref array 9)
                       :imag-max (aref array 4)))

(defun imag-min (window)
  (with-slots (center radius) window
    (- (imagpart center) (imagpart radius))))

(defun imag-max (window)
  (with-slots (center radius) window
    (+ (imagpart center) (imagpart radius))))

(defun real-min (window)
  (with-slots (center radius) window
    (- (realpart center) (realpart radius))))

(defun real-max (window)
  (with-slots (center radius) window
    (+ (realpart center) (realpart radius))))

(defun cursor-position-to-complex (cpos window)
  (let* ((x-pos (car cpos))
         (y-pos (cadr cpos))
         (win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size))
         (real-mouse (ju:map-val x-pos 0.0 cur-width (real-min window) (real-max window)))
         (imag-mouse (ju:map-val (- cur-height y-pos) 0.0 cur-height (imag-min window) (imag-max window))))
    (complex real-mouse imag-mouse)))

(defun zoom-complex-fractal-window (scale cpos fractal)
  (with-slots (center radius) fractal
    (let* ((new-radius (* radius scale))
           (new-center (cursor-position-to-complex cpos fractal)))
      (setf center new-center
            radius new-radius))))

(defun pan-complex-fractal-window (offset-percent fractal)
  (with-slots (radius center) fractal
    (incf center (complex (* (realpart radius) (realpart offset-percent))
                          (* (imagpart radius) (imagpart offset-percent))))))


#+spacenav
(defmethod sgl:handle-3d-mouse-event ((object complex-window) (event sn:motion-event))
  (with-slots (sn:x sn:z sn:y) event
    (let ((zoom-in-percent (+ 1.0f0 (/ sn:y 5000.0)))
          (xm (/ sn:x 5000.0))
          (ym (/ sn:z 5000.0))
          (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size))))
      (zoom-complex-fractal-window zoom-in-percent window-center object)
      (pan-complex-fractal-window (complex xm ym) object)))
  (update-bounds object))

(defmethod handle-key ((object complex-window) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (iter-up-percent 1.10)
         (iter-down-percent 0.90)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (shift-down (find :shift mod-keys))
         (alt-down (find :alt mod-keys))
         (need-reload
           (with-slots (center radius) object
             (cond
               ((and (eq key :f5) (eq action :press))
                (setf center  #C(0.0f0 0.0f0))
                (setf radius  #C(4.0f0 4.0f0)))

               ((and (eq key :page-down)
                     (or (eq action :release)
                         (eq action :press)
                         (eq action :repeat)))
                (zoom-complex-fractal-window zoom-in-percent window-center object))

               ((and (eq key :page-up)
                     (or (eq action :release)
                         (eq action :press)
                         (eq action :repeat)))
                (zoom-complex-fractal-window zoom-out-percent window-center object))

               ((and shift-down (eq key :down) (or (eq action :press) (eq action :repeat)))
                (when (get-uniform object "cImag")
                  (let* ((uni (get-uniform object "cImag"))
                         (univ (get-value uni)))
                    (set-uniform object
                                 "cImag"
                                 (- univ
                                    (* (if alt-down
                                           0.01
                                           1)
                                       (abs radius)
                                       univ))
                                 :float))))


               ((and shift-down (eq key :up) (or (eq action :press) (eq action :repeat)))
                (when (get-uniform object "cImag")
                  (let* ((uni (get-uniform object "cImag"))
                         (univ (get-value uni)))
                    (set-uniform object
                                 "cImag"
                                 (+ univ
                                    (* (if alt-down
                                           0.01
                                           1)
                                       (abs radius)
                                       univ))
                                 :float))))


               ((and shift-down (eq key :left) (or (eq action :press) (eq action :repeat)))
                (when (get-uniform object "cReal")
                  (let* ((uni (get-uniform object "cReal"))
                         (univ (get-value uni)))
                    (set-uniform object
                                 "cReal"
                                 (- univ
                                    (* (if alt-down
                                           0.01
                                           1)
                                       (abs radius)
                                       univ))
                                 :float))))


               ((and shift-down (eq key :right) (or (eq action :press) (eq action :repeat)))
                (when (get-uniform object "cReal")
                  (let* ((uni (get-uniform object "cReal"))
                         (univ (get-value uni)))
                    (set-uniform object "cReal"
                                 (* univ
                                    (+ ) (* (if alt-down
                                              0.01
                                              1)
                                          (abs radius)
                                          univ))
                                 :float))))

               ;; ((and shift-down (eq key :down) (or (eq action :press) (eq action :repeat)))
               ;;  (when (get-uniform object "cImag")
               ;;    (format t "spanning~%")
               ;;    (set-uniform object "cImag" (- (get-uniform object "cImag") (* 0.1 (get-uniform object "cImag"))) :float)
               ;;    (set-uniform object "cReal" (- (get-uniform object "cReal") (* 0.1 (get-uniform object "cReal"))) :float)))

               ;; ((and shift-down (eq key :up) (or (eq action :press) (eq action :repeat)))
               ;;  (when (get-uniform object "cImag")
               ;;    (format t "spanning~%")
               ;;    (set-uniform object "cImag" (+ (get-uniform object "cImag") (* 0.1 (get-uniform object "cImag"))) :float)
               ;;    (set-uniform object "cReal" (+ (get-uniform object "cReal") (* 0.1 (get-uniform object "cReal"))) :float)))


               ((and shift-down (eq key :left) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

               ((and shift-down (eq key :right) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex pan-offset 0.0) object))


               ((and (eq key :down) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex 0.0 (- pan-offset)) object))

               ((and (eq key :up) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex 0.0 pan-offset) object))

               ((and (eq key :left) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

               ((and (eq key :right) (or (eq action :press) (eq action :repeat)))
                (pan-complex-fractal-window (complex pan-offset 0.0) object))

               ((and (eq key :equal) (or (eq action :press) (eq action :repeat)))
                (with-slots ( max-iterations ) object
                  (setf max-iterations (max 1 (1+ (floor (* max-iterations iter-up-percent)))))
                  (set-uniform object "maxIterations" max-iterations :int)))

               ((and (eq key :minus) (or (eq action :press) (eq action :repeat)))
                (with-slots ( max-iterations ) object
                  (setf max-iterations (max 1 (floor (* max-iterations iter-down-percent))))
                  (set-uniform object "maxIterations" max-iterations :int)))

               (t
                (call-next-method))))))
    (when need-reload
      (update-bounds object)
      t)))

(defclass complex-fractal-click (mouse-click)
  ((window :initarg :window)))

;; (defmethod sgl:handle-drag ((object complex-fractal) window (click complex-fractal-click) cursor-pos)
;;   (declare (ignorable window))

;;   (with-slots (sgl:vertices zoom-window) object
;;     (with-slots (center radius) zoom-window
;;       (incf center (- (cursor-position-to-complex (slot-value sgl:*previous-mouse-drag* 'sgl:cursor-pos)
;;                                                   zoom-window)
;;                       (cursor-position-to-complex cursor-pos zoom-window))))
;;     (setf sgl:vertices (to-vertices zoom-window))
;;     (sgl:initialize object)
;;     (with-slots (sgl:cursor-pos sgl:mod-keys sgl:action sgl:button sgl:time) click
;;     (setf sgl:*previous-mouse-drag* (make-instance 'complex-fractal-click
;;                                                      :window zoom-window
;;                                                      :cursor-pos cursor-pos
;;                                                      :mod-keys sgl:mod-keys
;;                                                      :action sgl:action
;;                                                      :button sgl:button
;;                                                      :time sgl:time)))))


;; (defmethod handle-click ((object complex-window) window click)
;;   (with-slots (zoom-window) object
;;     (with-slots (cursor-pos mod-keys action button time) click
;;       (let ((mp (make-instance 'complex-fractal-click
;;                                               :window zoom-window
;;                                               :cursor-pos cursor-pos
;;                                               :mod-keys sgl:mod-keys
;;                                               :action sgl:action
;;                                               :button sgl:button
;;                                               :time sgl:time)))
;;         (cond ((eq sgl:action :press)
;;                (setf sgl:*previous-mouse-drag* mp)
;;                (setf sgl:*mouse-press-info* mp)
;;                (setf sgl:*mouse-release-info* nil))

;;               ((eq sgl:action :release)
;;                (setf sgl:*previous-mouse-drag* nil)
;;                (setf sgl:*mouse-press-info* nil)
;;                (setf sgl:*mouse-release-info* mp)))
;;         t))))

(defmethod handle-scroll ((object complex-window) window cpos x-scroll y-scroll)
  (declare (ignorable window x-scroll y-scroll))
  (zoom-complex-fractal-window (if (< 0 y-scroll)
                                   0.95
                                   1.05)
                               cpos
                               object)
  (let* ((win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size)))
    (glfw:set-cursor-position (coerce (/ cur-width 2.0) 'double-float)
                              (coerce (/ cur-height 2.0) 'double-float)))
  (sgl:reload object)
  t)
