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

(defun random-rgba ()
  (max 0
       (min #16rFFffFFff
            (+ (random 140)
               (ash (random 72) 9)
               (ash (random 128) 16)
               (ash (+ 40 (random 64)) 24)
               ))))

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

;; (defmethod update ((object sgl-fft-viz) elapsed-seconds)
;;   (set-uniform object "time" elapsed-seconds :float)
;;   (with-slots (hex-grids state-idx) object
;;     (let* ((cur-grid (aref hex-grids state-idx))
;;            (next-grid (aref hex-grids (next-state-idx object)))
;;            (state-buffer (sgl:get-buffer object :states))
;;            (state-ptr (if state-buffer
;;                           (slot-value state-buffer 'pointer)
;;                           nil))
;;            (radii-buffer (sgl:get-buffer object :radii))
;;            (radii-ptr (if radii-buffer
;;                           (slot-value radii-buffer 'pointer)
;;                           nil))
;;            (coord (copy-structure (hg:min-hex cur-grid)))
;;            (cur-idx 0))
;;       (unless (and state-ptr radii-ptr)
;;         (format t "state-ptr was nil...~%")
;;         (error "state-ptr was nil."))
;;       (loop
;;         :while (< (hg:oddr-col coord) (hg:max-col cur-grid))
;;         :do
;;            (setf (hg:oddr-row coord) (hg:min-row cur-grid))
;;            (loop
;;              :while (< (hg:oddr-row coord) (hg:max-row cur-grid))
;;              :for pt = (hg:center coord)
;;              :do
;;                 (let* ((cur (hg:state cur-grid coord))
;;                        (neighbors (hg:neighbors coord))
;;                        (neighbor-sum (loop
;;                                        :for neigh :across neighbors
;;                                        :summing
;;                                        (if (= 1 (hg:state
;;                                                  cur-grid neigh))
;;                                            1
;;                                            0))))

;;                   (cond
;;                     ((and (or (zerop cur) (= 2 cur))
;;                           (= 2 neighbor-sum))
;;                      (setf (hg:state next-grid coord) 1))
;;                     ((and (= 1 cur)
;;                           (or
;;                            (= 2 neighbor-sum)
;;                            (= 5 neighbor-sum)
;;                            (= 6 neighbor-sum)))
;;                      (setf (hg:state next-grid coord) 1))
;;                     ((= 1 cur)
;;                      (setf (hg:state next-grid coord) 4))
;;                     (t
;;                      (setf (hg:state next-grid coord) 0)))
;;                   (setf (gl:glaref state-ptr cur-idx) (hg:state cur-grid coord))
;;                   (incf cur-idx)
;;                   (incf (hg:oddr-row coord))))
;;            (incf (hg:oddr-col coord)))
;;       (hg:swap-states object)
;;       (values radii-buffer state-buffer))))


(defmethod sgl-blend2d-texture:draw-image ((obj sgl-fft-viz) img ctx size)
  (declare (ignorable obj img ctx size))
  (let ((width size)
        (height size)
        (count 360)
        (jump-count 16)
        (t-min 0.0d0)
        (t-max (* 2 pi))
        (t-win (/ pi 4.5))
        (min-step 0.0001)
        (stroke-width 0.00175d0)
        (fx (lambda (tv) (* 1.0 (exp (cos tv)) (cos (* 2.5 tv)))))
        (fy (lambda (tv) (* 1.0 (exp  (sin tv)) (sin (* 2.5 tv)))))
        (rad 2.25d0)
        (nrad -2.25d0))

    (bl:with-objects
        ((path bl:path-core)
         (linear bl:linear-gradient-values)
         (grad bl:gradient-core))

      (setf (bl:linear-gradient-values.x0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.y0 linear) 0.0d0)
      (setf (bl:linear-gradient-values.x1 linear) (coerce width 'double-float))
      (setf (bl:linear-gradient-values.y1 linear) (coerce width 'double-float))

      (setup-window ctx nrad nrad rad rad width height)

      (dotimes (i count)
        (bl:gradient-init-as grad
                             bl:+gradient-type-linear+
                             linear
                             bl:+extend-mode-pad+
                             (cffi:null-pointer)
                             0
                             (cffi:null-pointer))
        (let ((first-rgba (random-rgba))
              (second-rgba (random-rgba)))
          (bl:gradient-add-stop-rgba32 grad 0.0d0 first-rgba)

          (bl:gradient-add-stop-rgba32 grad (* 2 pi) second-rgba))

        (bl:path-init path)

        (let* ((t-start (ju:random-between t-min t-max)))

          (bl:path-move-to path
                           (funcall fx t-start) (funcall fy t-start))
          (loop
            :for i fixnum :below jump-count
            :for t0 double-float = t-start then (+ t2 (random-between min-step t-win))
            :for t1 double-float = (+ t0 (random-between min-step t-win))
            :for t2 double-float = (+ t1 (random-between min-step t-win))
            :for x1 double-float = (funcall fx t0)
            :for y1 double-float = (funcall fy t0)
            :for x2 double-float = (funcall fx t1)
            :for y2 double-float = (funcall fy t1)
            :for x3 double-float = (funcall fx t2)
            :for y3 double-float = (funcall fy t2)
            :do
               (bl:path-cubic-to path x1 y1 x2 y2 x3 y3))
          (bl:context-set-comp-op ctx
                                  bl:+comp-op-src-over+)
          (bl:context-set-stroke-style ctx
                                       grad)
          (bl:context-set-stroke-width ctx
                                       (random-between 0.00001 stroke-width))
          (bl:context-set-stroke-cap ctx
                                     bl:+stroke-cap-position-start+
                                     bl:+STROKE-CAP-triangle+)
          (bl:context-set-stroke-cap ctx
                                     bl:+stroke-cap-position-end+
                                     bl:+STROKE-CAP-triangle+)
          (bl:context-set-stroke-join ctx bl:+stroke-join-round+)

          #+sbcl(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
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
;;       (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx (random-rgba)))
;;       (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle)))))
