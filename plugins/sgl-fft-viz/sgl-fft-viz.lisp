;; sgl-fft-viz.lisp

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

(in-package :sgl-fft-viz)

(setf sgl:*shader-dirs*
      (adjoin
       (asdf:system-relative-pathname :sgl-fft-viz "shaders/")
       sgl:*shader-dirs*))

(defclass sgl-fft-viz (sgl-blend2d-texture:sgl-blend2d-texture)
  ()
  (:documentation "Texture drawn by Blend2D."))

(defun setup-window (ctx x-min y-min x-max y-max width height)
  (let ((x-scale (/ width (- x-max x-min)))
        (y-scale (/ height (- y-max y-min)))
        (x-trans (- x-min))
        (y-trans (- y-min)))

    (cffi:with-foreign-array (arr (make-array 2 :initial-contents (list x-scale y-scale)) '(:array :double 2))
      (bl:context-matrix-op ctx bl:+matrix2d-op-scale+ arr))

    (cffi:with-foreign-array (arr (make-array 2 :initial-contents (list x-trans y-trans)) '(:array :double 2))
      (bl:context-matrix-op ctx bl:+matrix2d-op-translate+ arr))))

;; (defun setup-window (ctx x-min y-min x-max y-max width height)
;; (let ((x-scale (/ width
;;                   (- x-max x-min)))
;;       (y-scale (/ height
;;                   (- y-max y-min)))
;;       (x-trans (- x-min))
;;       (y-trans (- y-min)))
;;   (cffi:with-foreign-array (arr `#(,x-scale ,y-scale) '(:array :double 2))
;;     (bl:context-matrix-op ctx bl:+matrix2d-op-scale+ arr))

;;   (cffi:with-foreign-array (arr `#(,x-trans ,y-trans) '(:array :double 2))
;;     (bl:context-matrix-op ctx bl:+matrix2d-op-translate+ arr))))

(defmethod update ((object sgl-hex-grid) elapsed-seconds )
  (set-uniform object "time" elapsed-seconds :float)
  (with-slots (hex-grids state-idx) object
    (let* ((cur-grid (aref hex-grids state-idx))
           (next-grid (aref hex-grids (next-state-idx object)))
           (state-buffer (sgl:get-buffer object :states))
           (state-ptr (if state-buffer
                          (slot-value state-buffer 'pointer)
                          nil))
           (radii-buffer (sgl:get-buffer object :radii))
           (radii-ptr (if radii-buffer
                          (slot-value radii-buffer 'pointer)
                          nil))
           (coord (copy-structure (hg:min-hex cur-grid)))
           (cur-idx 0))
      (unless (and state-ptr radii-ptr)
        (format t "state-ptr was nil...~%")
        (error "state-ptr was nil."))
      (loop
        :while (< (hg:oddr-col coord) (hg:max-col cur-grid))
        :do
           (setf (hg:oddr-row coord) (hg:min-row cur-grid))
           (loop
             :while (< (hg:oddr-row coord) (hg:max-row cur-grid))
             :for pt = (hg:center coord)
             :do
                (let* ((cur (hg:state cur-grid coord))
                       (neighbors (hg:neighbors coord))
                       (neighbor-sum (loop
                                       :for neigh :across neighbors
                                       :summing
                                       (if (= 1 (hg:state
                                                 cur-grid neigh))
                                           1
                                           0))))

                  (cond
                    ((and (or (zerop cur) (= 2 cur))
                          (= 2 neighbor-sum))
                     (setf (hg:state next-grid coord) 1))
                    ((and (= 1 cur)
                          (or
                           (= 2 neighbor-sum)
                           (= 5 neighbor-sum)
                           (= 6 neighbor-sum)))
                     (setf (hg:state next-grid coord) 1))
                    ((= 1 cur)
                     (setf (hg:state next-grid coord) 4))
                    (t
                     (setf (hg:state next-grid coord) 0)))
                  (setf (gl:glaref state-ptr cur-idx) (hg:state cur-grid coord))
                  (incf cur-idx)
                  (incf (hg:oddr-row coord))))
           (incf (hg:oddr-col coord)))
      (hg:swap-states object)
      (values radii-buffer state-buffer))))

(defmethod sgl-blend2d-texture:draw-image ((obj sgl-fft-viz) img ctx size)
  (declare (ignorable obj img ctx size))
  (let ((width size)
        (height size)
        (count 100)
        (jump-count 3)
        (t-min 0.0d0)
        (t-max (* 2 pi))
        (t-win (/ pi 2))
        (stroke-width 0.002d0)
        (fx (lambda (tv) (* 1.5 (cos tv) (cos (* 7 tv)))))
        (fy (lambda (tv) (* 1.5 (sin tv) (cos (* 7 tv)))))
        )
    (bl:with-objects
        ((path bl:path-core)
         (linear bl:linear-gradient-values)
         (grad bl:gradient-core))

      (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.x1 linear) 0.0d0)
      (setf (bl:linear-gradient-values.y1 linear) (coerce width 'double-float))

      (setup-window ctx -2.0d0 -2.0d0 2.0d0 2.0d0 width height)

      (dotimes (i count)
        (bl:gradient-init-as grad
                             bl:+gradient-type-linear+
                             linear
                             bl:+extend-mode-pad+
                             (cffi:null-pointer)
                             0
                             (cffi:null-pointer))

        (bl:gradient-add-stop-rgba32 grad 0.0d0 (random #16rffffffff))

        (dotimes (stops (random 3))
          (bl:gradient-add-stop-rgba32 grad (random 1.0d0) (random #16rffffffff)))

        (bl:gradient-add-stop-rgba32 grad 1.0d0 (random #16rffffffff))

        (bl:path-init path)

        (let* ((t-start (ju:random-between t-min t-max)))
          (bl:path-move-to path
                           (funcall fx t-start) (funcall fy t-start))
          (loop
            :for i :below jump-count
            :for t0 = t-start then (+ t2 (random t-win))
            :for t1 = (+ t0 (random t-win))
            :for t2 = (+ t1 (random t-win))
            :do
               (bl:path-cubic-to path
                                 (funcall fx t0) (funcall fy t0)
                                 (funcall fx t1) (funcall fy t1)
                                 (funcall fx t2) (funcall fy t2)))
          (bl:context-set-comp-op ctx
                                  bl:+comp-op-src-over+)
          (bl:context-set-stroke-style ctx
                                       grad)
          (bl:context-set-stroke-width ctx
                                       stroke-width)
          (bl:context-set-stroke-cap ctx
                                     bl:+stroke-cap-position-start+
                                     bl:+stroke-cap-round+)
          (bl:context-set-stroke-cap ctx
                                     bl:+stroke-cap-position-end+
                                     bl:+stroke-cap-round+)

          #+sbcl (sb-int:with-float-traps-masked
                     (:invalid)
                   (bl:context-stroke-geometry ctx
                                               bl:+geometry-type-path+
                                               path))
          #-sbcl (bl:context-stroke-geometry ctx
                                             bl:+geometry-type-path+
                                             path)


          (bl:path-reset path)
          (bl:gradient-reset grad))))))

;;   (bl:with-objects
;;   ((circle bl:circle))
;; (dotimes (i 500)
;;     (let* ((sx (random (coerce size 'double-float)))
;;            (sy (random (coerce size 'double-float)))
;;            (radius (coerce (random (/ size 20.0)) 'double-float)))

;;       (setf (bl:circle.cx circle) sx)
;;       (setf (bl:circle.cy circle) sy)
;;       (setf (bl:circle.r circle) radius)
;;       (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
;;       (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random #16rffffffff)))
;;       (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)))))
