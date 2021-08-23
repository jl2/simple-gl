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
   (width :initform 50 :initarg :width :type fixnum)
   (rule :initform 90 :initarg :rule :type fixnum)
   (current-row :initform 0 :initarg :current-row :type fixnum)
   (current-row-data :initform nil :initarg :current-row-data :type (or null (SIMPLE-ARRAY BIT (*))))
   (next-row-data :initform nil :initarg :next-row-data :type (or null (SIMPLE-ARRAY BIT (*))))
   ))

(defun create-cellular-automata (width
                                 &key
                                   (rule 90)
                                   (max-instances (* width 1000))
                                   (initial-data))
  "Create a cellular automata of the specified size."
  (let* ((init-data (if (null initial-data)
                        (make-array (list width) :initial-element 0 :element-type 'bit)
                        (coerce initial-data '(simple-array bit (*)))))
         (ret-val (make-instance 'cellular-automata
                                 :max-instances max-instances
                                 :width width
                                 :rule rule
                                 :current-row 0
                                 :current-row-data init-data
                                 :next-row-data (make-array (list width) :initial-element 0 :element-type 'bit))))
    (with-slots (current-row-data next-row-data width) ret-val
      (when (null initial-data)
        (setf (aref current-row-data (floor (/ width 2))) 1)))
    ret-val))

(defun add-row-instances (object)
  "Draw the next row of automata data by adding translations to the instance buffer."

  (with-slots (buffers instance-count max-instances width current-row current-row-data) object
    (let ((buffer (get-buffer object :obj-transform))
          (cell-size (/ 2.0 width)))
      (with-slots (pointer) buffer
        ;; Loop over each cell in the data
        (loop
          ;; Track whether any quads have been added
          with updated = nil

          ;; Calculate the quad location
          for x-offset fixnum from 0
          for x-float real = (- 1.0 (* cell-size x-offset))
          for y-offset fixnum = current-row
          for y-float real = (1- (* 2 (/ (1- y-offset) width)))

          ;; Bail out when there's no room for a new quad.
          until (>= instance-count max-instances)

          ;; If the cell is 'on' then add a quad
          for rv bit across current-row-data
          when (= 1 rv) do
                (sgl:fill-pointer-offset (vec3 x-float y-float 0.0)
                                         pointer
                                         (* instance-count 3))
                (setf updated t)
                (incf instance-count)

          finally
             ;; Copy the buffer to OpenGL if anything changed.
             (when updated
               (reload buffer)))))))

(declaim (inline apply-rule left-element right-element compute-next-row add-row-instance))

(defun apply-rule (rule lb cb rb)
  "Compute output cell value based on left, center, and right cells."
  (declare (type bit lb cb rb)
           (type fixnum rule))
  (let ((idx (+ (* 4 lb) (* 2 cb) rb)))
    (if (logbitp idx rule)
        1
        0)))

;; Override these to simulate different board types (wrap around, etc.)
(defgeneric left-element (ca idx)
  (:documentation "Look up the cell value 'left' of idx."))

(defgeneric right-element (ca idx)
  (:documentation "Look up the cell value 'right' of idx."))

(defmethod left-element ((ca cellular-automata) idx)
  (with-slots (current-row-data width) ca
    (let ((real-idx (if (= idx 0)
                        (1- width)
                        (1- idx))))
      (aref current-row-data real-idx))))

;; This implementation wraps around to the other side of the board
(defmethod right-element ((ca cellular-automata) idx)
  (with-slots (current-row-data width) ca
    (let ((real-idx (if (= idx (1- width))
                        0
                        (1+ idx))))
      (aref current-row-data real-idx))))

(defgeneric compute-next-row (object)
  (:documentation "Compute the next row of the automata."))

(defmethod compute-next-row ((object cellular-automata))
  (with-slots (instance-count rule max-instances width current-row current-row-data next-row-data) object
    (loop for idx fixnum from 0
          for left-bit bit = (left-element object idx)
          for cur-bit bit across current-row-data
          for right-bit bit = (right-element object idx)
          do
             (setf (aref next-row-data idx) (apply-rule rule left-bit cur-bit right-bit)))
    (rotatef current-row-data next-row-data)
    (incf current-row)))

(defmethod update ((object cellular-automata) elapsed-seconds)
  (declare (ignorable elapsed-seconds))
  (with-slots (max-instances instance-count) object
    (when (< instance-count max-instances)
      (add-row-instances object)
      (compute-next-row object))))

(defmethod initialize-buffers ((object cellular-automata) &key)
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
                                                      (vec3 0.0 0.0 0.0))
                                :stride nil
                                :attributes '(("translation" . :vec3))
                                :usage :static-draw
                                :free nil))))
