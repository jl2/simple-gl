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
  (with-slots (buffers instance-count max-instances node level
               current-iteration generated-iteration) object
    (when (/= current-iteration generated-iteration)

      (let ((buffer (get-buffer object :obj-transform)))
        (setf instance-count 0)
        (with-slots (pointer) buffer
          (let* ((real-node (hl:advance node current-iteration))
                 (offset (/ (hl:node-size real-node) 2)))
            ;;multiple-value-bind (min-x min-y max-x max-y) (hl:find-bounds real-node level)

            ;;(declare (ignorable max-x max-y))
            (hl:for-each-cell real-node
                              level
                              (lambda (x y gray)
                                (let ((pt (vec3 (- x offset)
                                                (- y offset)
                                                gray)))

                                  (when (and (< instance-count max-instances))
                                    (sgl:fill-pointer-offset pt
                                                             pointer
                                                             (* instance-count 3))
                                    (incf instance-count))))
                              0
                              0)))
        (setf generated-iteration current-iteration)
        buffer))))



(defun create-hashlife (filename-or-pts-or-node &key
                                   (color (vec4 0.1 0.9 0.1 1.0))
                                   (max-instances (* 100 100)))
  "Read a game of life initial position from a file."
  (make-instance 'hashlife
                 :max-instances max-instances
                 :color color
                 :node (etypecase filename-or-pts-or-node
                         (string (hl:make-hashlife filename-or-pts-or-node))
                         (list (hl:construct filename-or-pts-or-node))
                         (hl:qtnode filename-or-pts-or-node))))

(defmethod compute-next ((object hashlife))
  (with-slots (node current-iteration) object
    (hl:advance node (* 2 current-iteration))))
