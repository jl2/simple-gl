;; create-viewers.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:sgl-fractal)

(defun mandelbrot-viewer (&key (max-iterations 120))
  (let ((viewer (make-instance 'complex-fractal-viewer)))
    (sgl:add-object viewer
                    :mandelbrot (make-instance 'complex-window
                                               :max-iterations max-iterations
                                               :styles (list
                                                        (cons :mandelbrot
                                                              (make-style-from-files
                                                               "complex-vertex.glsl"
                                                               "mandel-fragment.glsl")))))
    viewer))

(defun cubic-mandelbrot-viewer (&key (max-iterations 120))
  (let ((viewer (make-instance 'complex-fractal-viewer)))
    (sgl:add-object viewer
                    :cubic-mandelbrot
                    (make-instance 'complex-window
                           :max-iterations max-iterations
                           :styles (list
                                            (cons :cubic-mandelbrot
                                                  (make-style-from-files
                                                   "complex-vertex.glsl"
                                                   "cubic-mandel-fragment.glsl")))))
    viewer))

(defun julia-set-viewer (&key (max-iterations 120) (real 0.324f0) (imag -0.2345))
  (let ((cw (make-instance 'complex-window
                           :max-iterations max-iterations
                           :styles (list
                                    (cons :julia-set
                                          (make-style-from-files
                                           "complex-vertex.glsl"
                                           "julia-set-fragment.glsl")))))
        (viewer (make-instance 'complex-fractal-viewer)))
    (sgl:set-uniform cw "cReal" real :float)
    (sgl:set-uniform cw "cImag" imag :float)
    (sgl:add-object viewer :julia-set cw)
    viewer))

(defun cubic-julia-set-viewer (&key (max-iterations 120) (real 0.324f0) (imag -0.2345))
  (let ((cw (make-instance 'complex-window
                           :max-iterations max-iterations
                           :styles (list
                                    (cons :cubic-julia-set
                                          (make-style-from-files
                                           "complex-vertex.glsl"
                                           "cubic-julia-set-fragment.glsl")))))
        (viewer (make-instance 'complex-fractal-viewer)))
    (sgl:set-uniform cw "cReal" real :float)
    (sgl:set-uniform cw "cImag" imag :float)
    (sgl:add-object viewer :cubic-julia-set cw)
    viewer))

(defun burning-ship-viewer (&key (max-iterations 120))
  (let ((viewer (make-instance 'complex-fractal-viewer)))
    (sgl:add-object viewer :burning-ship
                    (make-instance 'complex-window
                                   :max-iterations max-iterations
                                   :styles (list
                                            (cons :burning-ship
                                                  (make-style-from-files
                                                   "complex-vertex.glsl"
                                                   "burning-ship-fragment.glsl")))))
    viewer))

(defun cubic-burning-ship-viewer (&key (max-iterations 120))
  (let ((viewer (make-instance 'complex-fractal-viewer)))
    (sgl:add-object viewer :cubic-burning-ship
                    (make-instance 'complex-window
                                   :max-iterations max-iterations
                                   :styles (list
                                            (cons :cubic-burning-ship
                                                  (make-style-from-files
                                                   "complex-vertex.glsl"
                                                   "cubic-burning-ship-fragment.glsl")))))
    viewer))

(defun bs-js-viewer (&key (max-iterations 120) (real 0.324f0) (imag -0.2345))
  (let ((cw (make-instance 'complex-window
                           :max-iterations max-iterations
                           :styles (list
                                    (cons :burning-ship-julia-set
                                          (make-style-from-files
                                           "complex-vertex.glsl"
                                           "bs-js-fragment.glsl")))))
        (viewer (make-instance 'complex-fractal-viewer)))
    (sgl:set-uniform cw "cReal" real :float)
    (sgl:set-uniform cw "cImag" imag :float)
    (sgl:add-object viewer :bs-js cw)
    viewer))

(defun cubic-bs-js-viewer (&key (max-iterations 120) (real 0.324f0) (imag -0.2345))
  (let ((cw (make-instance 'complex-window
                           :max-iterations max-iterations
                           :styles (list
                                    (cons :cubic-burning-ship-julia-set
                                          (make-style-from-files
                                           "complex-vertex.glsl"
                                           "cubic-bs-js-fragment.glsl")))))
        (viewer (make-instance 'complex-fractal-viewer)))
    (sgl:set-uniform cw "cReal" real :float)
    (sgl:set-uniform cw "cImag" imag :float)
    (sgl:add-object viewer :cubic-bs-js cw)
    viewer))
