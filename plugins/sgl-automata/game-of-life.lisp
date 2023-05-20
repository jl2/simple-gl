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

(defclass game-of-life (cellular-automata)
  ((pts :initarg :pts)
   (level :initarg :level :initform 0)))

(defun in-box (hlp min-pt max-pt)
  (and (< (hl:pt-x hlp) (vx max-pt))
       (> (hl:pt-x hlp) (vx min-pt))
       (< (hl:pt-y hlp) (vy max-pt))
       (> (hl:pt-y hlp) (vy min-pt))))

(defmethod add-current-instances ((object game-of-life))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (buffers instance-count max-instances pts min-pt max-pt
               current-iteration generated-iteration) object
    (when (/= current-iteration generated-iteration)
      (let ((buffer (get-buffer object :obj-transform))
            (cell-width 1.0
                        )
            (cell-height 1.0
                         ))
        (declare (type single-float cell-width cell-height))
        (setf instance-count 0)
        (with-slots (pointer) buffer
          (loop
            :for icount :below max-instances
            :for pt :in (hl:baseline-advance pts current-iteration)
            :for x-float = (* cell-width (hl::pt-x pt))
            :for y-float = (* cell-height (hl::pt-y pt))
            :when (in-box pt min-pt max-pt)
              :do
                 (sgl:fill-pointer-offset (vec3 x-float y-float (hl:pt-gray pt))
                                          pointer
                                          (* icount 3))
                 (incf instance-count )))
        (setf generated-iteration current-iteration)
        buffer))))

(defun create-random-game-of-life (width height
                                   &key
                                     (max-instances (* width height)))
  "Create a cellular automata of the specified size."
  (make-instance 'game-of-life
                 :max-instances max-instances
                 :game (loop :for i :below (* width height)
                            :collecting (hl:pt (random width) (random height)))))

(defun create-game-of-life (filename &key
                                       (color (vec4 0.1 0.9 0.1 1.0))
                                       (max-instances (* 100 100)))
  "Read a game of life initial position from a file."
  (make-instance 'game-of-life
                 :max-instances max-instances
                 :color color
                 :pts (hl:make-life filename)))

(defmethod compute-next ((object game-of-life)))
