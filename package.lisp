;; package.lisp
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

(setf *features* (remove :3D-VECTORS-DOUBLE-FLOATS *features*))

(defpackage :simple-gl

  (:nicknames #:sgl)

  (:use #:cl
        #:alexandria
        #:3d-vectors
        #:3d-matrices)

  (:export #:*shader-dirs*  ;; The search paths for shaders

           ;; Shared methods
           #:cleanup
           #:update
           #:show-info

           ;; Viewer
           #:viewer
           #:3d-mouse-nav-viewer
           #:display-in

           ;; OpenGL objects
           #:opengl-object
           #:initialize-buffers
           #:initialize-uniforms
           #:initialize-textures
           #:primitive-type
           #:buffers
           #:get-buffer
           #:set-buffer
           #:get-uniform
           #:set-uniform
           #:bind
           #:render

           ;; Instanced OpenGL objects (inherits from opengl-object)
           #:instanced-opengl-object
           #:instance-count

           ;; Buffers
           #:buffer
           #:attribute-buffer
           #:index-buffer
           #:instance-buffer
           #:fill-pointer-offset
           #:reload
           #:pointer
           #:to-gl-array
           #:fill-buffer

           ;; Styles/shaders
           #:style
           #:gl-shader
           #:make-style-from-files
           #:build-style
           #:simple-gl-shader
           #:read-shader
           #:point-style

           ;; Textures
           #:texture
           #:fill-texture
           ))
