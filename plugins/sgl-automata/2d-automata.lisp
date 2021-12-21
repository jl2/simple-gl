;; 2d-automata.lisp
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

(defclass 2d-cellular-automata (cellular-automata)
  ((width :initform 50
          :initarg :width
          :type fixnum)
   (height :initform 50
           :initarg :height
           :type fixnum)
   (current-board-idx :initform 0
                      :initarg :current-board-idx
                      :type fixnum)
   (current-board-data :initform nil
                       :initarg :current-board-data
                       :type (or null (SIMPLE-ARRAY BIT (*))))
   (next-board-data :initform nil
                    :initarg :next-board-data
                    :type (or null (SIMPLE-ARRAY BIT (*))))))

(declaim (inline count-neighbors add-current-instances compute-next))

(defun create-2d-cellular-automata (width height
                                    &key
                                      (class 'sgl-automata:game-of-life)
                                      (max-instances (* width height))
                                      (initial-data (make-array (* width height)
                                                                :element-type 'bit
                                                                :initial-contents (loop for i below (* width height)
                                                                                        collecting (random 2)))))
  "Create a cellular automata of the specified size."
  (make-instance class
                 :width width
                 :height height
                 :current-board-idx 0
                 :max-instances max-instances
                 :current-board-data initial-data
                 :next-board-data (make-array (list (* width height))
                                              :initial-element 0
                                              :element-type 'bit)))
(declaim (inline 2d-get (setf 2d-get)))
(defun 2d-get (array height i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array bit (*)) array)
           (type (unsigned-byte 32) height i j))
  (aref array (+ i (* height j) )))

(defun (setf 2d-get) (value array height i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (simple-array bit (*)) array)
           (type (unsigned-byte 32) height i j))
  (setf (aref array (+ i (* height j))) value))

(defmethod add-current-instances ((object 2d-cellular-automata))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (buffers instance-count max-instances height width current-board-idx current-board-data) object
    (let ((buffer (get-buffer object :obj-transform))
          (cell-width (/ 2.0f0 (coerce width 'single-float)))
          (cell-height (/ 2.0f0 height)))
      (declare (type single-float cell-width cell-height))
      (setf instance-count 0)
      (with-slots (pointer) buffer
        ;; Loop over each cell in the data
        (loop
          ;; Track whether any quads have been added
          with updated = nil

          ;; Calculate the quad location
          for x-offset from 0 below width
          for x-float real = (- 1.0f0 (* cell-width x-offset))
          do
             (loop
               for y-offset from 0 below height
               for y-float real = (- 1.0f0 (* cell-height y-offset))

               ;; If the cell is 'on' then add a quad
               when (is-on object x-offset y-offset) do
                 (sgl:fill-pointer-offset (vec3 x-float y-float 0.0f0) ;(* -0.1f0 current-board-idx))
                                          pointer
                                          (* instance-count 3))
                 (setf updated t)
                 (incf instance-count))
          finally
             ;; Copy the buffer to OpenGL if anything changed.
             (when updated
               (reload buffer)))))))


(defgeneric count-neighbors (object i j)
  (:documentation "Count neighbors at board location i j"))

(declaim (inline is-on is-off turn-on turn-off))

(defun is-on (object i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) i j)
           (type 2d-cellular-automata object))
  (with-slots (current-board-data height) object
    (= 1 (2d-get current-board-data height i j))))
(defun turn-on (object i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) i j)
           (type 2d-cellular-automata object))
  (with-slots (next-board-data width height) object
    (setf (2d-get next-board-data height i j) 1)))

(defun is-off (object i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) i j)
           (type 2d-cellular-automata object))
  (with-slots (current-board-data height) object
    (= 0 (2d-get current-board-data height i j))))
(defun turn-off (object i j)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (unsigned-byte 32) i j)
           (type 2d-cellular-automata object))
  (with-slots (next-board-data height) object
    (setf (2d-get next-board-data height i j) 0)))

(defmethod count-neighbors ((object 2d-cellular-automata) i j)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type (unsigned-byte 32) i j))
  (with-slots (current-board-data width height) object
    (declare (type (unsigned-byte 32) width height))
    (let ((ip (if (> 0 (- i 1))
                  (1- width)
                  (1- i)))
          (jp (if (> 0 (1- j))
                  (- height 1)
                  (- j 1)))
          (in (if (>= i (- width 1))
                  0
                  (+ i 1)))
          (jn (if (>= j (- height 1))
                  0
                  (+ j 1))))
      (declare (type fixnum ip jp in jn))
      (+ 
       (2d-get current-board-data width ip jp)
       (2d-get current-board-data width ip j)
       (2d-get current-board-data width ip jn)
       (2d-get current-board-data width i jp)
       (2d-get current-board-data width i jn)
       (2d-get current-board-data width in jp)
       (2d-get current-board-data width in j)
       (2d-get current-board-data width in jn)))))

