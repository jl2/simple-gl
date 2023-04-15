;; obj-file.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

;; (use-package :obj-reader)

;; (defclass sgl-obj (opengl-object)
;;   ((filename :initarg :filename :type (or string pathname))
;;    (obj-file :initform nil :type (or null obj-file)))


;; (defmethod initialize-buffers ((obj sgl-obj) &key)
;;   (with-slots (filename obj-file) obj
;;     (when (null obj-file)
;;       (setf obj-file (read-obj-from-file file-name)))

;;     (with-slots (objects) obj-file

;;       (loop :for obj-object :in objects :do
;;         (with-slots (groups) obj-object
;;           (loop
;;             :for group :in groups
;;             :do
;;                (let ((object (make-instance 'obj-group :obj-group group)))
;;                  (with-slots (obj-reader:face-stride obj-reader:faces) group
;;                    (set-buffer object
;;                                :vertices
;;                                (make-instance
;;                                 'attribute-buffer
;;                                 :pointer (to-gl-array
;;                                           :float
;;                                           (length obj-reader:faces)
;;                                           obj-reader:faces))
;;                                :stride obj-reader:face-stride
;;                                :attributes (cons ((= 3 obj-reader:face-stride)
;;                                                   '(("in_position" . :vec3)))
;;                                                  ((= 5 obj-reader:face-stride)
;;                                                   '(("in_position" . :vec3) ("in_texture" . :vec2)))
;;                                                  ((= 6 obj-reader:face-stride)
;;                                                   '(("in_position" . :vec3) ("in_normal" . :vec3)))
;;                                                  ((= 8 obj-reader:face-stride)
;;                                                   '(("in_position" . :vec3) ("in_texture" . :vec2) ("in_normal" . :vec3)))
;;                                                  :usage :static-draw
;;                                                  :free t))
;;                    (set-buffer object
;;                                :obj-transform (make-instance
;;                                                'instance-buffer
;;                                                :pointer (to-gl-array :float (* 16 1)
;;                                                                      (list
;;                                                                       (meye 4))
;;                                                                      :stride 16
;;                                                                      :attributes '(("obj_transform" . :mat4))
;;                                                                      :usage :static-draw
;;                                                                      :free t))))
;;                  ))
;;           (let ((vert-data (gl:alloc-gl-array :float (+
;;           (cffi:with-pointer-to-vector-data (vert-ptr obj-reader:vertices)
;;             (set-buffer obj :vertices (make-instance 'attribute-buffer
;;                                                      :pointer (gl::make-gl-array-from-pointer vert-ptr :float (* 3 (length obj-reader:vertices)))
;;                                                      :attributes '(("in_position" . :vec3))
;;                                                      :free nil)))
;;           (cffi:with-pointer-to-vector-data (norm-ptr obj-reader:normals)
;;             (set-buffer obj :vertices (make-instance 'attribute-buffer
;;                                                      :pointer (gl::make-gl-array-from-pointer norm-ptr :float (* 3 (length obj-reader:normals)))
;;                                                      :attributes '(("in_normal" . :vec3))
;;                                                      :free nil)))
;;           (cffi:with-pointer-to-vector-data (tex-ptr obj-reader:tex-coords)
;;             (set-buffer obj :vertices (make-instance 'attribute-buffer
;;                                                      :pointer (gl::make-gl-array-from-pointer tex-ptr :float (* 2 (length obj-reader:tex-coords)))
;;                                                      :attributes '(("in_tex" . :vec2))
;;                                                      :free nil))))))))
