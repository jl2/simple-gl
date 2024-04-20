;; package.lisp
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

(setf *features* (remove :3D-VECTORS-DOUBLE-FLOATS *features*))

(defpackage :simple-gl

  (:nicknames #:sgl)

  (:use #:cl
        #:alexandria
        #:3d-vectors
        #:3d-matrices)

  (:export #:*shader-dirs*  ;; The search paths for shaders

           #:new-sgl-plugin

           #:deg2rad
           #:rad2deg

           ;; Shared methods
           #:cleanup
           #:update
           #:show-info
           #:uniforms
           #:buffers
           #:program
           #:uniforms
           #:primitive-type
           #:styles
           #:name
           #:vao
           #:gl-fset
           #:gl-get

           ;; Viewer
           #:viewer
           #:2d-viewer
           #:3d-viewer
           #:radius
           #:φ
           #:θ
           #:zoom
           #:pan
           #:rotation
           #:center-pt
           #:update-all-view-transforms-safe
           #:add-object
           #:rm-object
           #:replace-object
           #:objects
           #:pause

           #:handle-3d-mouse-event
           #:handle-key

           #:with-viewer-lock
           #:*display-in-main-thread*
           #:reset-view
           #:reset-view-safe
           #:offscreen-viewer
           #:output-directory
           #:initial-height
           #:initial-width
           #:view-matrix
           #:display
           #:*viewers*
           #:find-viewer
           #:view-changed


           ;; OpenGL objects
           #:opengl-object
           #:initialize-buffers
           #:initialize-uniforms
           #:initialize-textures
           #:ensure-initialized
           #:primitive-type
           #:buffers
           #:get-buffer
           #:set-buffer
           #:get-uniform
           #:use-uniform
           #:set-uniform
           #:get-value
           #:bind
           #:render

           #:stl-file

           ;; Instanced OpenGL objects (inherits from opengl-object)
           #:instanced-opengl-object
           #:instance-count

           #:quad

           ;; Buffers
           #:buffer
           #:attribute-buffer
           #:constant-attribute-buffer
           #:constant-instance-buffer
           #:constant-index-buffer
           #:index-buffer
           #:idx-count
           #:instance-buffer
           #:fill-pointer-offset
           #:pointer
           #:to-gl-array
           #:fill-buffer
           #:show-gl-array
           #:from-gl-array
           #:rebuild-style
           #:refill-textures
           #:reload-buffers

           ;; Styles/shaders
           #:style
           #:gl-shader
           #:make-style-from-files
           #:use-style
           #:build-style
           #:find-gl-shader
           #:read-shader
           #:point-style

           ;; Textures
           #:texture
           #:fill-texture
           #:png-texture

           ))
