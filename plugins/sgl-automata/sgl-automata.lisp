;; sgl-automata.lisp
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(in-package :sgl-automata)

(setf sgl:*shader-dirs*
      (adjoin (asdf:system-relative-pathname :sgl-automata "shaders/") sgl:*shader-dirs*))


(defclass cellular-automata (instanced-opengl-object)
  ((style :initform (make-style "automata" "sgl-automata-vertex.glsl" "point-fragment.glsl"))
   (max-instances :initform 10000 :initarg :max-instances :type fixnum)
   (instance-count :initform 0 :type fixnum)
   ))

(defclass cell-renderer (cellular-automata)
  ())

(defclass line-renderer (cellular-automata)
  ())

(declaim (inline apply-rule left-element right-element compute-next-row add-row-instance))

(defgeneric add-current-instances (object)
  (:documentation "Add instances for the current generation of an automata."))

(defgeneric compute-next (object)
  (:documentation "Compute the next instance  of the automata."))


(defmethod update ((object cellular-automata) elapsed-seconds)
  (declare (ignorable elapsed-seconds))
  (with-slots (max-instances instance-count) object
    (when (< instance-count max-instances)
      (add-current-instances object)
      (compute-next object))))

(defmethod initialize-buffers ((object cell-renderer) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))

  ;; Fill vertex and index buffers with data for a single OpenGL quad
  (with-slots (width) object
    (let ((cell-width (/ 2.0f0 width)))
      (set-buffer object
                  :vertices
                  (make-instance
                   'attribute-buffer
                   :pointer (to-gl-array
                             :float
                             28
                             (list 0.0f0 0.0f0 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0
                               cell-width 0.0f0 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0

                               cell-width cell-width 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0

                               0.0f0 cell-width 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0))
                   :stride nil
                   :attributes '(("in_position" . :vec3)
                                 ("in_color" . :vec4))
                   :usage :dynamic-draw
                   :free nil))))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2 0 2 3))
               :stride nil
               :usage :static-draw
               :free t))

  ;; Create an empty instance buffer that will be filled with vec3 translations
  (with-slots (max-instances instance-count) object
    (setf instance-count 0)
    (set-buffer object
                :obj-transform (make-instance
                                'instance-buffer
                                ;; Not technically empty, but this gets overwritten immediately
                                :pointer (to-gl-array :float
                                                      (* max-instances 3)
                                                      (vec3 0.0f0 0.0f0 0.0f0))
                                :stride nil
                                :attributes '(("translation" . :vec3))
                                :usage :static-draw
                                :free nil))))

(defmethod initialize-buffers ((object line-renderer) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))

  ;; Fill vertex and index buffers with data for a single OpenGL quad
  (with-slots (width) object
    (let ((cell-width (/ 2.0f0 width)))
      (set-buffer object
                  :vertices
                  (make-instance
                   'attribute-buffer
                   :pointer (to-gl-array
                             :float
                             28
                             (list 0.0f0 0.0f0 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0
                               cell-width 0.0f0 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0

                               cell-width cell-width 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0

                               0.0f0 cell-width 0.0f0
                               0.1f0 0.8f0 0.1f0 1.0f0))
                   :stride nil
                   :attributes '(("in_position" . :vec3)
                                 ("in_color" . :vec4))
                   :usage :dynamic-draw
                   :free nil))))
  (set-buffer object
              :indices
              (make-instance
               'index-buffer
               :idx-count 6
               :pointer (to-gl-array :unsigned-int 6 #(0 1 2 0 2 3))
               :stride nil
               :usage :static-draw
               :free t))

  ;; Create an empty instance buffer that will be filled with vec3 translations
  (with-slots (max-instances instance-count) object
    (setf instance-count 0)
    (set-buffer object
                :obj-transform (make-instance
                                'instance-buffer
                                ;; Not technically empty, but this gets overwritten immediately
                                :pointer (to-gl-array :float
                                                      (* max-instances 3)
                                                      (vec3 0.0f0 0.0f0 0.0f0))
                                :stride nil
                                :attributes '(("translation" . :vec3))
                                :usage :static-draw
                                :free nil))))
