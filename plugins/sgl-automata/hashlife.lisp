;; game-of-life.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

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

(defclass sgl-hashlife (cellular-automata)
  ())

(defclass sgl-slowlife (cellular-automata)
  ((pts :initform nil :initarg :pts :type list)))


(defun naive-life (file-name)
  "Create a cellular automata of the specified size."
  (make-instance 'sgl-slowlife :pts (hl:read-life-file file-name)))

(defmethod compute-next ((object sgl-slowlife))
  (with-slots (instance-count pts) object
    (let ((new-pts (hl:baseline-life pts)))
      (setf pts new-pts))))


(defmethod add-current-instances ((object sgl-slowlife))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (pts buffers instance-count max-instances) object
    (let ((buffer (get-buffer object :obj-transform))
          (cell-width 0.5)
          (cell-height 0.5))
      (setf instance-count 0)
      (with-slots (pointer) buffer
        ;; Loop over each cell
        (loop
          ;; Track whether any quads have been added
          :with updated = nil
          :for (px . py) :in pts
          
          ;; Calculate the quad location
          :for x-float real = (- 1.0f0 (* cell-width px))
          :for y-float real = (- 1.0f0 (* cell-height py))
          :do
          
             ;; If the cell is 'on' then add a quad
             (format t "Adding quad ~a ~a~%" x-float y-float)
             (sgl:fill-pointer-offset
              (vec3 x-float y-float 0.0f0)
                                        ;(* -0.1f0 current-board-idx))
              pointer
              (* instance-count 3))
             (setf updated t)
             (incf instance-count)
          :finally
             ;; Copy the buffer to OpenGL if anything changed.
             (return buffer)
             ;; (when updated
             ;;   (return buffer))
          )))))
