;; style.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass style ()
  ((name :initform "Default Style"
         :initarg :name
         :accessor name)
   (enabled :initform t
            :initarg :enabled
            :accessor enabledp)
   (program :initform 0
            :accessor program
            :type fixnum)
   (shaders :initform nil
           :type (or null list)
           :accessor shaders
            :initarg :shaders)))

(defmethod update ((style style) elapsed-seconds)
  (declare (ignorable style elapsed-seconds))
  nil)

(defmethod show-info ((style style) &key (indent 0))
  (let ((this-ws (indent-whitespace indent))
        (plus-ws (indent-whitespace (+ 1 indent)))
        (plus-plus-ws (indent-whitespace (+ 2 indent))))
    (format t "~aStyle:~%" this-ws)
    (show-slots plus-ws style '(name shaders program ))

    (with-slots (program) style
      (format t "~aProgram ~a:~%" plus-ws program)
      (dolist (attrib '(:delete-status
                        :link-status
                        :validate-status
                        :info-log-length
                        :attached-shaders
                        :active-atomic-counter-buffers
                        :active-attributes
                        :active-attribute-max-length
                        :active-uniforms
                        :active-uniform-blocks
                        :active-uniform-block-max-name-length
                        :active-uniform-max-length
                        ;; :compute-work-group-size
                        :program-binary-length
                        :transform-feedback-buffer-mode
                        :transform-feedback-varyings
                        :transform-feedback-varying-max-length
                        ;; :geometry-vertices-out
                        ;; :geometry-input-type
                        ;; :geometry-output-type
                        ))
        (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-program program attrib)))
      (dotimes (idx (gl:get-program program :active-attributes))
        (multiple-value-bind (size type name) (gl:get-active-attrib program idx)
          (format t "~aAttrib ~a ~a size ~a~%" plus-plus-ws name type size)))


      (let ((gl-shaders (gl:get-attached-shaders program)))
        (format t "~aOpenGL Shaders: ~a~%" plus-ws gl-shaders)
        (dolist (shader gl-shaders)
        (format t "~aShader ~a~%" plus-ws shader)
        (dolist (attrib '(:shader-type
                          :delete-status :compile-status :info-log-length :shader-source-length))
          (format t "~a~a : ~a~%" plus-plus-ws attrib (gl:get-shader shader attrib))))))))

(defmethod cleanup ((style style))
  (with-slots (shaders program) style
    (when (not (zerop program))
      (gl:delete-program program)
      (setf program 0))
    (when shaders
      (dolist (shade shaders)
        (with-slots (shader) shade
          (when (and (not (zerop shader))
                     (not (zerop program)))
            (gl:detach-shader program shader)))
        (cleanup shade)))))

(define-condition shader-link-error (shader-error) ())
(define-condition shader-validate-error (shader-error) ())

(defmethod build-style ((style style))
  "Compile and link a shader program, including validation."

  (with-slots (program shaders) style
    ;; Compile all shaders
    (when (zerop program)
      (setf program (gl:create-program)))

    (dolist (gl-shader shaders)
      (when (and (not (zerop (shader gl-shader))) (not (zerop program)))
        (gl:detach-shader program (shader gl-shader)))
      (initialize gl-shader)
      (gl:attach-shader program (shader gl-shader)))

    ;; Link
    (gl:link-program program)

    ;; Check for errors and validate program
    (let ((status (gl:get-program program :link-status)))
      (when (not (eq t status))
        (error 'shader-link-error
               :status status
               :object program
               :info-log (gl:get-program-info-log program))))

    (gl:validate-program program)
    (let ((status (gl:get-program program :validate-status)))
      (when (not (eq t status))
        (restart-case
            (error 'shader-link-error
                   :status status
                   :object program
                   :info-log (gl:get-program-info-log program))
          (ignore-validation-error () t))))
    program))

(defclass wireframe-style (style)
  ((shaders :initform (list (read-shader "position-vertex.glsl")
                            (read-shader "black-fragment.glsl")))))

(defgeneric use-style (style)
  (:documentation "Apply style settings."))
(defgeneric unuse-style (style)
  (:documentation "Unapply style settings."))

(defmethod use-style ((style style))
  (with-slots (program) style
    (gl:polygon-mode :front-and-back
                     :fill)
    (gl:use-program program))
  t)

(defmethod unuse-style ((style style))
  t)

(defmethod use-style ((style wireframe-style))
  (gl:polygon-mode :front-and-back :line)
  (gl:enable :polygon-offset-line)
  (gl:polygon-offset 1.0 (/ 1.0 10))
  (with-slots (program) style
    (gl:use-program program))

  t)

(defmethod unuse-style ((style wireframe-style))
  ;; (gl:polygon-mode :front-and-back
  ;;                  :fill)
  ;; (gl:disable :polygon-offset-line)
  )

(defun make-style (name type &rest shaders)
  (if shaders
      (make-instance type :shaders (mapcar #'read-shader shaders) :name name)
      (make-instance type :name name)))

(defun point-style ()
  (make-style "point" 'style "position-color-vertex.glsl" "point-fragment.glsl"))

(defun plastic-style ()
  (make-style "plastic" 'style
              "position-normal-color-vertex.glsl"
              "plastic-fragment.glsl"))

(defun normal-style ()
  (make-style "normal" 'style "position-normal-vertex.glsl" "normal-fragment.glsl"))

(defun position-style ()
  (make-style "position" 'style "position-vertex.glsl" "position-fragment.glsl"))

(defun painted-plastic-style ()
  (make-style "painted" 'style "position-normal-uv-vertex.glsl" "textured-plastic-fragment.glsl"))

(defun circled-plastic-style ()
  (make-style "circled" 'style "position-normal-uv-vertex.glsl" "circle-fragment.glsl"))

(defun wireframe-style ()
  (make-style "wireframe" 'wireframe-style))

(defun simple-texture-style ()
  (make-style "simple-texture" "position-uv-vertex.glsl" "simple-texture-fragment.glsl"))

