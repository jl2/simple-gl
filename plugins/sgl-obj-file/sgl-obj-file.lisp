;; sgl-obj-file.lisp
;; Copyright (c) 2024 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

;; Permission to use, copy, modify, and/or distribute this software for any
;; purpose with or without fee is hereby granted, provided that the above
;; copyright notice and this permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(in-package :sgl-obj-file)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-obj-file "shaders/") sgl:*shader-dirs*))

(defclass sgl-obj (sgl:instanced-opengl-object)
  ((sgl:styles :initform (list
                          `(:obj-style
                            .
                            ,(sgl:make-style-from-files "obj-vertex.glsl"
                                                        "obj-plastic-fragment.glsl")))
               :initarg :styles)
   (filename :initarg :filename
             :type (or string pathname))
   (obj-file :initform nil
             :type (or null obj-reader:obj-file))
   (transforms :initform (list (meye 4))
               :initargs :transforms)))

(defun ensure-loaded (sgl-obj)
  (with-slots (obj-file filename transforms sgl:instance-count) sgl-obj
    (when (null obj-file)
      (setf obj-file (obj:read-obj-from-file filename)))))

(defmethod sgl:initialize-buffers ((obj sgl-obj) &key)
  (with-slots (obj-file filename transforms sgl:instance-count) obj
    (ensure-loaded obj)
    (with-slots ((objs obj:objects) obj:materials) obj-file
      (loop
        :for object :in objs
        :do
           (with-slots (obj:object-name
                        obj:groups
                        obj:normals
                        obj:tex-coords
                        obj:vertices
                        obj:v-params) object
             ;; (format t "Filling buffers for ~a~%" obj:object-name)
             (let ((attribute-data (make-array (+ (* 3 (length obj:vertices)))
                                               :element-type 'float
                                               :adjustable t
                                               :initial-element 0.0
                                               :fill-pointer 0))
                   (index-count 0)
                   (idx-format nil))

               (loop
                 :for group :across obj:groups
                 :do
                    (with-slots (obj:faces obj:group-name obj:lines obj:material obj:points obj:smoothing-group) group
                      (format t "Filling buffer for ~a~%" obj:group-name)
                      (loop
                        :for face :across obj:faces
                        :do (with-slots (obj:indices
                                         obj:idx-format) face
                              (setf idx-format obj:idx-format)
                              (labels ((idx-idx (component offset idx stride)
                                         (+ component
                                            (* stride
                                               (aref obj:indices (+ offset
                                                                    idx)))))
                                       (add-vertex (idx offset)
                                         (vector-push-extend (aref obj:vertices (idx-idx 0 offset idx 3)) attribute-data)
                                         (vector-push-extend (aref obj:vertices (idx-idx 1 offset idx 3)) attribute-data)
                                         (vector-push-extend (aref obj:vertices (idx-idx 2 offset idx 3)) attribute-data))

                                       (add-normal (idx offset)
                                         (vector-push-extend (aref obj:normals (idx-idx 0 offset idx 3)) attribute-data)
                                         (vector-push-extend (aref obj:normals (idx-idx 1 offset idx 3)) attribute-data)
                                         (vector-push-extend (aref obj:normals (idx-idx 2 offset idx 3)) attribute-data))

                                       (add-tex (idx offset)
                                         (vector-push-extend (aref obj:tex-coords (idx-idx 0 offset idx 2)) attribute-data)
                                         (vector-push-extend (aref obj:tex-coords (idx-idx 1 offset idx 2)) attribute-data))

                                       (add-vdata (idx offset)
                                         (vector-push-extend (aref obj:v-params (idx-idx 0 offset idx 1)) attribute-data)))

                                (loop
                                  :for idx :below (length obj:indices) :by (obj:stride face)
                                  :do
                                     (incf index-count)
                                     (case obj:idx-format
                                       (:vertex-texture-normal-vdata

                                        (add-vertex idx 0)
                                        (add-tex idx 1)
                                        (add-normal idx 2)
                                        (add-vdata idx 3))

                                       (:vertex-texture-normal
                                        (add-vertex idx 0)
                                        (add-tex idx 1)
                                        (add-normal idx 2))

                                       (:vertex-texture
                                        (add-vertex idx 0)
                                        (add-tex idx 1))

                                       (:vertex-normal
                                        (add-vertex idx 0)
                                        (add-normal idx 1))

                                       (:vertex
                                        (add-vertex idx 0)))))))))
               ;;                (format t "idx-format: ~a~%" idx-format)
               (sgl:set-buffer
                obj
                :vertices (sgl:constant-attribute-buffer
                           attribute-data
                           (length attribute-data)
                           (case idx-format
                             (:vertex-texture-normal-vdata
                              '(("in_position" . :vec3)
                                ("in_texture" . :vec2)
                                ("in_normal" . :vec3)
                                ("in_vdata" . :float)))
                             (:vertex-texture-normal
                              '(("in_position" . :vec3)
                                ("in_texture" . :vec2)
                                ("in_normal" . :vec3)))
                             (:vertex-texture
                              '(("in_position" . :vec3)
                                ("in_texture" . :vec2)))
                             (:vertex-normal
                              '(("in_position" . :vec3)
                                ("in_normal" . :vec3)))
                             (:vertex-vdata
                              '(("in_position" . :vec3)
                                ("in_vdata" . :float)))
                             (:vertex
                              '(("in_position" . :vec3))))
                           :free nil))
               (sgl:set-buffer obj :indices (sgl:constant-index-buffer index-count
                                                                       :free nil))
               (setf sgl:instance-count 1)
               (sgl:set-buffer obj :obj-transform (sgl:constant-instance-buffer transforms
                                                                                (* 16 (length transforms))
                                                                                '(("obj_transform" . :mat4))
                                                                                :free nil
                                                                                ))))))))

(defmethod sgl:initialize-uniforms ((object sgl-obj) &key)
  (with-slots (obj-file filename transforms) object
    (when (null obj-file)
      (setf obj-file (obj:read-obj-from-file filename)))
    (sgl:set-uniform object "view_transform" (meye 4) :mat4)
    (with-slots ((objs obj:objects) (mats obj:materials)) obj-file
      (loop
        :for obj :in objs
        :do
           (with-slots (obj:groups) obj

             (loop
               :for group :across obj:groups
               :do
                  (with-slots (obj:material) group
                    (format t "obj:material: ~a~%" obj:material)
                    (with-slots (obj:attributes) (gethash obj:material mats)
                      (format t "obj:attributes: ~a~%" obj:attributes)
                      (loop
                        :for uniform-name
                          :being :the hash-keys
                            :of obj:attributes
                              :using (hash-value value)
                        :do
                           ;; (format t "  ~a: ~a (~a)~%" uniform-name value (type-of value))
                           (typecase value
                               (list
                                ;; (format t "List~a~%" (length value))
                                (cond
                                  ((= 4 (length value))
                                   (sgl:set-uniform object uniform-name (apply #'vec4 value) :vec4))
                                  ((= 3 (length value))
                                   (if (string= uniform-name "Kd")
                                       (sgl:set-uniform object uniform-name (vec4 (car value)
                                                                                  (cadr value)
                                                                                  (caddr value)
                                                                                  1.0) :vec4)
                                       (sgl:set-uniform object uniform-name (apply #'vec3 value) :vec3)))
                                  ((= 2 (length value))
                                   (sgl:set-uniform object uniform-name (apply #'vec2 value) :vec2))
                                  ((= 1 (length value))
                                   (sgl:set-uniform object uniform-name (car value) :float))))
                             (t (sgl:set-uniform object uniform-name value :float)))
                        ))))))
      t)))
