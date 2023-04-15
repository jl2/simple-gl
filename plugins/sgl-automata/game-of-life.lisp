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


(defclass game-of-life (cellular-automata)
  ((pts :initarg :pts :type list)
   (iterator :initform #'hl:iterate-baseline-life :initarg :iterator)))

(defmethod add-current-instances ((object game-of-life))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (buffers instance-count max-instances pts) object
    (let ((buffer (get-buffer object :obj-transform))
          (cell-width 0.8 ;;(/ 2.0f0 (coerce (1+  (- max-x min-x)) 'single-float))
                      )
          (cell-height 0.8 ;; (/ 2.0f0 (1+ (- max-y min-y)))
                       ))
      (declare (type single-float cell-width cell-height))
      (setf instance-count 0)
      (with-slots (pointer) buffer
        ;; Loop over each cell in the data
        ;; (format t "min-x ~a  max-x ~a  min-y ~a  max-y ~a  cell-width ~a  cell-height ~a~%" min-x max-x  min-y max-y cell-width cell-height)
        ;; (lparallel:pmap
        ;;  nil
        ;;  (lambda (pt)
        ;;    (let ((x-float (- 1.0f0 (* cell-width (car pt))))
        ;;          (y-float (- 1.0f0 (* cell-height (cdr pt)))))
        ;;      (sgl:fill-pointer-offset (vec3 x-float y-float 0.0f0)
        ;;                               pointer
        ;;                               (* instance-count 3))))
        ;;  pts)
        (loop
          :for pt :in pts
          :for x-float = (- 1.0f0 (* cell-width (car pt)))
          :for y-float = (- 1.0f0 (* cell-height (cdr pt)))
          :do
             (sgl:fill-pointer-offset (vec3 x-float y-float 0.0f0)
                                      pointer
                                      (* instance-count 3)))
        (setf instance-count (length pts)))
      
      ;; (loop
      ;;   :for (x . y) :in pts
      ;;   :for x-float real = (- 1.0f0 (* cell-width x))
      ;;   :for y-float real = (- 1.0f0 (* cell-height y))
      ;;   :do
      ;;      ;;(format t "x ~a y ~a xf ~a  yf ~a~%" x y x-float y-float)
      ;;      (sgl:fill-pointer-offset (vec3 x-float y-float 0.0f0)
      ;;                               pointer
      ;;                               (* instance-count 3))
      ;;      (incf instance-count)))
      buffer)))

(defun create-random-game-of-life (width height
                                   &key
                                     (iterator #'hl:iterate-baseline-life)
                                     (max-instances (* width height)))
  "Create a cellular automata of the specified size."
  (make-instance 'game-of-life
                 :max-instances max-instances
                 :iterator iterator
                 :pts (loop :for i :below (* width height)
                            :collecting (cons (random width) (random height)))))

(defun create-game-of-life (filename &key
                                       (iterator #'hl:iterate-baseline-life)
                                       (max-instances (* 100 100)))
  "Create a cellular automata of the specified size."
  (make-instance 'game-of-life
                 :max-instances max-instances
                 :iterator iterator
                 :pts (hl:read-game-file filename)))

(defmethod compute-next ((object game-of-life))
  (with-slots (instance-count iterator pts) object
    (setf pts (funcall iterator pts))
;;    (format t "pts: ~a~%" pts)
    (setf instance-count (length pts))))
