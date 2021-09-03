;; 1d-automata.lisp
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

(defclass 1d-cellular-automata (cellular-automata)
  ((width :initform 50 :initarg :width :type fixnum)
   (rule :initform 90 :initarg :rule :type fixnum)
   (current-row-idx :initform 0 :initarg :current-row-idx :type fixnum)
   (current-row-data :initform nil :initarg :current-row-data :type (or null (SIMPLE-ARRAY BIT (*))))
   (next-row-data :initform nil :initarg :next-row-data :type (or null (SIMPLE-ARRAY BIT (*))))
   ))

(defclass 1d-cellular-automata-wrapping (1d-cellular-automata)
  ())

;; Override these to simulate different board types (wrap around, etc.)
(defgeneric left-element (ca idx)
  (:documentation "Look up the cell value 'left' of idx."))

(defgeneric right-element (ca idx)
  (:documentation "Look up the cell value 'right' of idx."))


(defun create-1d-cellular-automata (width
                                    &key
                                      (class '1d-cellular-automata)
                                      (rule 90)
                                      (max-instances (* width 1000))
                                      (initial-data))
  "Create a cellular automata of the specified size."
  (let* ((init-data (if (null initial-data)
                        (make-array (list width) :initial-element 0 :element-type 'bit)
                        (coerce initial-data '(simple-array bit (*)))))
         (ret-val (make-instance class
                                 :max-instances max-instances
                                 :width width
                                 :rule rule
                                 :current-row-idx 0
                                 :current-row-data init-data
                                 :next-row-data (make-array (list width) :initial-element 0 :element-type 'bit))))
    (with-slots (current-row-data next-row-data width) ret-val
      (when (null initial-data)
        (setf (aref current-row-data (floor (/ width 2))) 1)))
    ret-val))



(defmethod add-current-instances ((object 1d-cellular-automata))
  "Draw the next row of automata data by adding translations to the instance buffer."

  (with-slots (buffers instance-count max-instances width current-row-idx current-row-data) object
    (let ((buffer (get-buffer object :obj-transform))
          (cell-size (/ 2.0f0 width)))
      (with-slots (pointer) buffer
        ;; Loop over each cell in the data
        (loop
          ;; Track whether any quads have been added
          with updated = nil

          ;; Calculate the quad location
          for x-offset fixnum from 0
          for x-float real = (- 1.0f0 (* cell-size x-offset))
          for y-offset fixnum = current-row-idx
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

(defun apply-rule (rule lb cb rb)
  "Compute output cell value based on left, center, and right cells."
  (declare (type bit lb cb rb)
           (type fixnum rule))
  (let ((idx (+ (* 4 lb) (* 2 cb) rb)))
    (if (logbitp idx rule)
        1
        0)))



(defmethod left-element ((ca 1d-cellular-automata-wrapping) idx)
  (with-slots (current-row-data width) ca
    (let ((real-idx (if (= idx 0)
                        (1- width)
                        (1- idx))))
      (aref current-row-data real-idx))))

;; This implementation wraps around to the other side of the board
(defmethod right-element ((ca 1d-cellular-automata-wrapping) idx)
  (with-slots (current-row-data width) ca
    (let ((real-idx (if (= idx (1- width))
                        0
                        (1+ idx))))
      (aref current-row-data real-idx))))



(defmethod left-element ((ca 1d-cellular-automata) idx)
  (with-slots (current-row-data width) ca
    (if (= idx 0)
        0
        (aref current-row-data (1- idx)))))

;; This implementation wraps around to the other side of the board
(defmethod right-element ((ca 1d-cellular-automata) idx)
  (with-slots (current-row-data width) ca
    (if (= idx (1- width))
        0
        (aref current-row-data (1+ idx)))))


(defmethod compute-next ((object 1d-cellular-automata))
  (with-slots (instance-count rule max-instances width current-row-idx current-row-data next-row-data) object
    (loop for idx fixnum from 0
          for left-bit bit = (left-element object idx)
          for cur-bit bit across current-row-data
          for right-bit bit = (right-element object idx)
          do
             (setf (aref next-row-data idx) (apply-rule rule left-bit cur-bit right-bit)))
    (rotatef current-row-data next-row-data)
    (incf current-row-idx)))
