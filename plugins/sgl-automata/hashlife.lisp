;; hashlife.lisp
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

(defclass hashlife (cellular-automata)
  ((node :initarg :node :type hl:qtnode)
   (level :initarg :level :initform 0 :type fixnum)))


(defmethod add-current-instances ((object hashlife))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (buffers instance-count max-instances node level min-pt max-pt
               current-iteration generated-iteration) object
    (when (/= current-iteration generated-iteration)

      (let ((buffer (get-buffer object :obj-transform))
            (cell-width 1.0)
            (cell-height 1.0))
        (declare (type single-float cell-width cell-height))
        (setf instance-count 0)
        (with-slots (pointer) buffer
          (let ((pts (hl:expand (hl:advance node current-iteration)
                                :level level)))
            (multiple-value-bind (min-x min-y max-x max-y) (hl:game-bounds pts)
              (declare (ignorable max-x max-y))
              (loop
                :for icount :below max-instances
                :for pt :in pts
                :for moved-pt = (hl:pt (- (hl:pt-x pt) min-x)
                                       (- (hl:pt-y pt) min-y))
                :for x-float = (* cell-width (hl:pt-x moved-pt))
                :for y-float = (* cell-height (hl:pt-y moved-pt))
                :for real-pt = (vec3 x-float y-float (hl:pt-gray pt))
                :when (in-box moved-pt min-pt max-pt)
                  :do
                     (sgl:fill-pointer-offset real-pt
                                              pointer
                                              (* icount 3))))
            (setf instance-count (min max-instances
                                      (length pts)))))
        (setf generated-iteration current-iteration)
        buffer))))



(defun create-hashlife (filename &key
                                       (color (vec4 0.1 0.9 0.1 1.0))
                                       (max-instances (* 100 100)))
  "Read a game of life initial position from a file."
  (make-instance 'hashlife
                 :max-instances max-instances
                 :color color
                 :node (hl:make-hashlife filename)))

(defmethod compute-next ((object hashlife)))
