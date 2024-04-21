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
  ((sgl:styles :initform (list (cons :stl-style
                                 (sgl:make-style-from-files "obj-vertex.glsl" "obj-plastic-fragment.glsl"))))
   (filename :initarg :filename
             :type (or string pathname))
   (obj-file :initform nil
             :type (or null obj-file))
   (transforms :initform (list (meye 4))
               :initargs :transforms)))

(defmethod sgl:initialize-buffers ((obj sgl-obj) &key)
  (with-slots (obj-file filename transforms) obj
    (when (null obj-file)
      (setf obj-file (obj:read-obj-from-file filename)))
    (with-slots ((objs obj:objects) obj:materials) obj-file
      (loop
        :for object :in objs
        :do (with-slots (obj:object-name
                         obj:groups
                         obj:normals
                         obj:tex-coords
                         obj:vertices
                         obj:v-params) object
              (format t "Filling buffers for ~a~%" obj:object-name)
              ;; TODO: Create attribute
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
                               (flet ((add-vertex (idx)
                                        (vector-push-extend (aref obj:vertices (+ 0 idx)) attribute-data)
                                        (vector-push-extend (aref obj:vertices (+ 1 idx)) attribute-data)
                                        (vector-push-extend (aref obj:vertices (+ 2 idx)) attribute-data))

                                      (add-normal (idx offset)
                                        (vector-push-extend (aref obj:normals (+ 0 (+ offset idx))) attribute-data)
                                        (vector-push-extend (aref obj:normals (+ 1 (+ offset idx))) attribute-data)
                                        (vector-push-extend (aref obj:normals (+ 2 (+ offset idx))) attribute-data))

                                      (add-tex (idx)
                                        (vector-push-extend (aref obj:tex-coords (+ 0 (+ 1 idx))) attribute-data)
                                        (vector-push-extend (aref obj:tex-coords (+ 1 (+ 1 idx))) attribute-data))

                                      (add-vdata (idx)
                                        (vector-push-extend (aref obj:v-params (+ 0 (+ 4 idx))) attribute-data)))
                                 (loop
                                   :for idx :below (length obj:indices) :by (obj:stride face)
                                   :do
                                      (incf index-count)
                                      (case obj:idx-format
                                        (:vertex-texture-normal-vdata

                                         (add-vertex idx)
                                         (add-tex idx)
                                         (add-normal idx 2)
                                         (add-vdata idx))

                                        (:vertex-texture-normal
                                         (add-vertex idx)
                                         (add-tex idx)
                                         (add-normal idx 2))

                                        (:vertex-texture
                                         (add-vertex idx)
                                         (add-tex idx))

                                        (:vertex-normal
                                         (add-vertex idx)
                                         (add-normal idx 1))

                                        (:vertex
                                         (add-vertex idx)))))))))

                (sgl:set-buffer obj :vertices (sgl:constant-attribute-buffer
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
                ;;(sgl:to-gl-array :float (length attribute-data) attribute-data)
                (sgl:set-buffer obj :indices (sgl:constant-index-buffer index-count
                                                                        :free nil))
                (setf (slot-value obj 'sgl:instance-count) 1)
                (sgl:set-buffer obj :obj-transform (sgl:constant-instance-buffer transforms
                                                                                 (* 16 (length transforms))
                                                                                 '(("obj_transform" . :mat4))
                                                                                 :free nil
                                                                                 ))))))))
