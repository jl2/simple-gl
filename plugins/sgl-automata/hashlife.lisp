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

(defun in-vbox (hlp min-pt max-pt)
  (and (< (vx hlp) (vx max-pt))
       (> (vx hlp) (vx min-pt))
       (< (vy hlp) (vy max-pt))
       (> (vy hlp) (vy min-pt))))

(defmethod add-current-instances ((object hashlife))
  "Draw the next row of automata data by adding translations to the instance buffer."
  (with-slots (buffers instance-count max-instances node level min-pt max-pt
               current-iteration generated-iteration) object
    (when (/= current-iteration generated-iteration)

      (let ((buffer (get-buffer object :obj-transform)))
        (setf instance-count 0)
        (with-slots (pointer) buffer
          (multiple-value-bind (min-x min-y max-x max-y) (hl:find-bounds node level)
            (declare (ignorable max-x max-y))

            (hl:for-each-cell (hl:ffwd node current-iteration)
                              level
                              (lambda (x y gray)
                                (let ((pt (vec3 x
                                                y
                                                gray)))

                                  (when (and (< instance-count max-instances)
                                             (in-vbox pt min-pt max-pt))
                                    (sgl:fill-pointer-offset pt
                                                             pointer
                                                             (* instance-count 3))
                                    (incf instance-count))))
                              (- (ash 1 (- (hl:q-k node) 1)))
                              (- (ash 1 (- (hl:q-k node) 1))))))
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
