;; instanced-opengl-object.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass instanced-opengl-object (opengl-object)
  ((styles :initform (list (cons :instanced-point-style (point-style-instanced)))
           :type (or null list)
           :accessor styles
           :initarg :styles)
   (max-instances :initform 100
                  :initarg :max-instances
                  :documentation "Maximum number of instances.  This is used to allocate space ahead of time.")
   (instance-count :initform 0
                   :initarg :instance-count
                   :documentation "Current instance count."))
  (:documentation "A pool of identical objects using OpenGL instancing.  Instance data is passed in buffers."))


(defmethod show-info ((object instanced-opengl-object) &key (indent 0))
  (call-next-method)
  (let ((plus-ws (indent-whitespace (+ 1 indent))))
    (show-slots plus-ws object '(instance-count))))

(defmethod render ((object instanced-opengl-object))
  (with-slots (buffers uniforms primitive-type instance-count styles) object
    (bind object)
    (loop :for (nil . style) :in styles :do
      (use-style style)
      (loop :for (nil . uniform) :in uniforms :do
        (use-uniform uniform (program style)))

      (when (> instance-count 0)
        ;; (format t "idx-count: ~a instance-count: ~a~%" (idx-count (assoc-value buffers :indices)) instance-count)
        (gl:draw-elements-instanced primitive-type

                                    (gl:make-null-gl-array :unsigned-int)

                                    instance-count
                                    :count (idx-count (assoc-value buffers :indices)))))))

(defmethod initialize-buffers ((object instanced-opengl-object) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))
  (set-buffer object
              :vertices
              (constant-attribute-buffer (list (vec3 -0.5f0 0.0f0 -0.5f0)
                                               (vec3 0.5f0 0.0f0 -0.5f0)
                                               (vec3 0.0f0 0.0f0 0.5f0))
                                         9
                                         '(("in_position" . :vec3))))

  (set-buffer object
              :obj-color (constant-attribute-buffer
                          (list (vec4 0.8 0.1 0.1 1.0)
                                (vec4 0.1 0.8 0.1 1.0)
                                (vec4 0.1 0.1 0.8 1.0))
                          (* 3 4)
                          '(("in_color" . :vec4))))

  (set-buffer object
              :obj-transform (constant-instance-buffer (list
                                                         (mtranslation (vec3 -1.0 0.0 0.0))
                                                         (meye 4)
                                                         (mtranslation (vec3 1.0 0.0 0.0)))
                                                        (* 16 3)
                                                        '(("obj_transform" . :mat4))))
  (set-buffer object
              :indices
              (constant-index-buffer 3)))

(defmethod initialize-uniforms ((object instanced-opengl-object) &key)
  (declare (ignorable object))
  (set-uniform object "view_transform" (meye 4) :mat4)
  t)
