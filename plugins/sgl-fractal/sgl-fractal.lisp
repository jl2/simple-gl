;; sgl-fractal.lisp
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

(in-package :sgl-fractal)

(defclass fractal-viewer (viewer)
  ())

#+spacenav(defmethod sgl:handle-3d-mouse-event ((object complex-window) (event sn:motion-event))
  (with-slots (sn:x sn:z sn:y) event
    (let ((zoom-in-percent (+ 1.0f0 (/ sn:y 5000.0)))
          (xm (/ sn:x 5000.0))
          (ym (/ sn:z 5000.0))
          (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size))))
      (zoom-complex-fractal-window zoom-in-percent window-center object)
      (pan-complex-fractal-window (complex xm ym) object)))
  (update-bounds object))

(defmethod sgl:handle-key ((object complex-window) window key scancode action mod-keys)
  (declare (ignorable window key scancode action mod-keys))
  (let* ((pan-offset 0.025)
         (zoom-in-percent 1.05)
         (zoom-out-percent 0.95)
         (iter-up-percent 1.10)
         (iter-down-percent 0.90)
         (window-center (mapcar (rcurry #'/ 2.0) (glfw:get-window-size)))
         (need-reload
          (cond ((and (eq key :f5) (eq action :press))
                 (with-slots (center radius) object
                   (setf center  #C(0.0f0 0.0f0))
                   (setf radius  #C(4.0f0 4.0f0))))

                ((and (eq key :page-down)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                ((and (eq key :page-down)  (eq action :release))
                 (zoom-complex-fractal-window zoom-in-percent window-center object))

                ((and (eq key :page-up)  (or (eq action :press) (eq action :repeat)))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                ((and (eq key :page-up)  (eq action :release))
                 (zoom-complex-fractal-window zoom-out-percent window-center object))

                ((and (eq key :down) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 (- pan-offset)) object))

                ((and (eq key :up) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex 0.0 pan-offset) object))

                ((and (eq key :equal) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (1+ (floor (* max-iterations iter-up-percent)))))
                   (set-uniform object "maxIterations" max-iterations :int)))

                ((and (eq key :minus) (or (eq action :press) (eq action :repeat)))
                 (with-slots ( max-iterations ) object
                   (setf max-iterations (max 1 (floor (* max-iterations iter-down-percent))))
                   (set-uniform object "maxIterations" max-iterations :int)))

                ((and (eq key :left) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex (- pan-offset) 0.0) object))

                ((and (eq key :right) (or (eq action :press) (eq action :repeat)))
                 (pan-complex-fractal-window (complex pan-offset 0.0) object))

                (t
                 (call-next-method)
                 nil))))
    (when need-reload
      (update-bounds object)
      t)))

(defclass complex-fractal-click (mouse-click)
  ((window :initarg :window)))

;; (defmethod sgl:handle-drag ((object complex-fractal) window (click complex-fractal-click) cursor-pos)
;;   (declare (ignorable window))

;;   (with-slots (sgl:vertices zoom-window) object
;;     (with-slots (center radius) zoom-window
;;       (incf center (- (cursor-position-to-complex (slot-value sgl:*previous-mouse-drag* 'sgl:cursor-pos)
;;                                                   zoom-window)
;;                       (cursor-position-to-complex cursor-pos zoom-window))))
;;     (setf sgl:vertices (to-vertices zoom-window))
;;     (sgl:initialize object)
;;     (with-slots (sgl:cursor-pos sgl:mod-keys sgl:action sgl:button sgl:time) click
;;     (setf sgl:*previous-mouse-drag* (make-instance 'complex-fractal-click
;;                                                      :window zoom-window
;;                                                      :cursor-pos cursor-pos
;;                                                      :mod-keys sgl:mod-keys
;;                                                      :action sgl:action
;;                                                      :button sgl:button
;;                                                      :time sgl:time)))))


;; (defmethod handle-click ((object complex-window) window click)
;;   (with-slots (zoom-window) object
;;     (with-slots (cursor-pos mod-keys action button time) click
;;       (let ((mp (make-instance 'complex-fractal-click
;;                                               :window zoom-window
;;                                               :cursor-pos cursor-pos
;;                                               :mod-keys sgl:mod-keys
;;                                               :action sgl:action
;;                                               :button sgl:button
;;                                               :time sgl:time)))
;;         (cond ((eq sgl:action :press)
;;                (setf sgl:*previous-mouse-drag* mp)
;;                (setf sgl:*mouse-press-info* mp)
;;                (setf sgl:*mouse-release-info* nil))

;;               ((eq sgl:action :release)
;;                (setf sgl:*previous-mouse-drag* nil)
;;                (setf sgl:*mouse-press-info* nil)
;;                (setf sgl:*mouse-release-info* mp)))
;;         t))))

(defmethod handle-scroll ((object complex-window) window cpos x-scroll y-scroll)
  (declare (ignorable window x-scroll y-scroll))
  (zoom-complex-fractal-window (if (< 0 y-scroll)
                                   0.95
                                   1.05)
                               cpos
                               object)
  (let* ((win-size (glfw:get-window-size))
         (cur-width (car win-size))
         (cur-height (cadr win-size)))
    (glfw:set-cursor-position (coerce (/ cur-width 2.0) 'double-float)
                              (coerce (/ cur-height 2.0) 'double-float)))
  (sgl:reload object)
  t)

;; (defun show-fractals ()
;;   (sgl:display (sgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (sgl:julia-set-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674)))
;;   (sgl:display (sgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (sgl:mandelbrot-set-shaders :max-iterations 1000)))
;;   (sgl:display (sgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (sgl:burning-ship-shaders :max-iterations 1000)))
;;   (sgl:display (sgl:make-st-quad
;;                   :s-min -2.0f0 :t-min -1.8f0
;;                   :s-max 2.0f0 :t-max 1.8f0
;;                   :shaders (sgl:bs-js-shaders :max-iterations 1000 :real 0.35453458392313 :imag 0.393437674))))
