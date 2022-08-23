;; game-of-life.lisp
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

;; (sgl:display-in
;;  (sgla:create-2d-cellular-automata 256 256)
;;  (make-instance 'sgl:viewer
;;                 :desired-fps 120))

;; (tmt:with-body-in-main-thread
;;     ()
;;   (sgl:display-in
;;    (sgla:create-game-of-life 512 512
;;                              :max-instances (* 512 512))
;;    (make-instance 'sgl:viewer
;;                   :desired-fps 120
;;                   :xform (m* (mperspective 60.0 1.0 0.1 1000.0)
;;                              (mlookat (vec3 1.5 1.5 2.0)
;;                                       (vec3 0 0 0)
;;                                       +vy+)))))


(defclass game-of-life (2d-cellular-automata)
  ())

(defun create-game-of-life (width height
                            &key
                              (max-instances (* width height))
                              (initial-data (make-array (* width height)
                                                        :element-type 'bit
                                                        :initial-contents
                                                        (loop for i below (* width height)
                                                              collecting (random 2)))))
  "Create a cellular automata of the specified size."
  (make-instance 'game-of-life
                 :width width
                 :height height
                 :current-board-idx 0
                 :max-instances max-instances
                 :current-board-data initial-data
                 :next-board-data (make-array (list (* width height))
                                              :initial-element 0
                                              :element-type 'bit)))

(defmethod compute-next ((object game-of-life))
  (with-slots (instance-count width height current-board-idx current-board-data next-board-data) object
    (loop
      ;; Calculate the quad location
      for i fixnum from 0 below width
      do
         (loop
           for j fixnum from 0 below height
           for ncount = (count-neighbors object i j)
           do
              (cond ((and (is-off object i j)
                          (= ncount 3))
                     (turn-on object i j))
                    ((and (is-on object i j)
                          (or (= ncount 2)
                              (= ncount 3)))
                     (turn-on object i j))
                    (t
                     (turn-off object i j)))))
    (rotatef next-board-data current-board-data)
    (incf current-board-idx)))


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
          :with updated = nil

          ;; Calculate the quad location
          :for x-offset from 0 below width
          :for x-float real = (- 1.0f0 (* cell-width x-offset))
          :do
             (loop
               :for y-offset :from 0 :below height
               :for y-float real = (- 1.0f0 (* cell-height y-offset))

               ;; If the cell is 'on' then add a quad
               :when (is-on object x-offset y-offset) do
                 (sgl:fill-pointer-offset (vec3 x-float y-float 0.0f0) ;(* -0.1f0 current-board-idx))
                                          pointer
                                          (* instance-count 3))
                 (setf updated t)
                 (incf instance-count))
          :finally
             ;; Copy the buffer to OpenGL if anything changed.
             (return buffer))))))
