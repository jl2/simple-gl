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
  ((last-time :initform 0.0 :type real))
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
            (+ (ju:random-between 128 255)
               (ash (ju:random-between 128 255) 8)
               (ash (ju:random-between 128 255) 16)
               (ash (ju:random-between 64 128) 24)
               ))))

(defmethod sgl:update ((object sgl-fft-viz) elapsed-seconds)
  (setf (slot-value object 'last-time) elapsed-seconds)
  object)

(defmethod handle-3d-mouse-event ((object sgl-fft-viz) (event sn:motion-event))
  (declare (ignorable object event))
  (format t "here we are: ~a~%" event)
  nil)


(defmethod sgl-blend2d-texture:draw-image ((obj sgl-fft-viz) img ctx size)
  (declare (ignorable obj img ctx size))
  (with-slots (last-time) obj
    (let* ((width (elt size 0))
           (height (elt size 1))
           (count 40)
           (jump-count 20)
           (t-min (* -2  pi))
           (t-max (*  2  pi ))
           (real-time (*  0.0005 last-time))
           (t-win (/ pi 12.0))
           (min-step 0.0001)
           (stroke-width 0.00365d0)
           (fx (lambda (tv) (* 2.0
                               (cos (* 2.3 (+  tv real-time)))
                               (cos (+ tv)))))
          (fy (lambda (tv) (* 2.0
                              (cos (* 2.3 (+  tv real-time)))
                              (sin (+ tv)))))
          (rad 2.25d0)
          (nrad -2.25d0))

      (bl:with-objects
          ((path bl:path-core)
           (linear bl:linear-gradient-values)
           (grad bl:gradient-core)
           (circle bl:circle))

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
              :for this-stroke-width = (random-between 0.000001 stroke-width)
              :do

                 (setf (bl:circle.cx circle) x1)
                 (setf (bl:circle.cy circle) y1)
                 (setf (bl:circle.r circle) (* 1.75 stroke-width))
                 (bl:lookup-error (bl:context-set-comp-op ctx bl:+comp-op-src-over+))
                 (bl:lookup-error (bl:context-set-fill-style-rgba32 ctx #16rbbff0000))
                 (bl:lookup-error (bl:context-fill-geometry ctx bl:+geometry-type-circle+ circle))
                 (bl:path-cubic-to path x1 y1 x2 y2 x3 y3)
                 (bl:context-set-comp-op ctx
                                         bl:+comp-op-src-over+)
                 (bl:context-set-stroke-style ctx
                                              grad)
                 (bl:context-set-stroke-width ctx
                                              this-stroke-width)
                 (bl:context-set-stroke-cap ctx
                                            bl:+stroke-cap-position-start+
                                            bl:+STROKE-CAP-triangle+)
                 (bl:context-set-stroke-cap ctx
                                            bl:+stroke-cap-position-end+
                                            bl:+STROKE-CAP-triangle+)
                 (bl:context-set-stroke-join ctx bl:+stroke-join-round+))

            #+sbcl(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
                       (bl:context-stroke-geometry ctx
                                                   bl:+geometry-type-path+
                                                   path))
            #-sbcl (bl:context-stroke-geometry ctx
                                                  bl:+geometry-type-path+
                                                  path)


            (bl:path-reset path)
            (bl:gradient-reset grad)))))))
