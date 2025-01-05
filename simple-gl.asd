;; simple-gl.asd
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

(asdf:defsystem #:simple-gl
  :description "A simple OpenGL view."
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (
               #+spacenav
               #:spacenav

               #:alexandria

               #:cl-glfw3
               #:cl-opengl

               #:3d-vectors
               #:3d-matrices

               #:ieee-floats
               #:local-time
               #:osicat
               #:zpng
               #:simple-png

               #:bordeaux-threads
               #:lparallel
               #:trivial-main-thread
               #:quickproject)

  :components ((:file "package")
               (:file "simple-gl")
               (:file "common")
               (:file "viewer")
               (:file "offscreen-viewer")
               (:file "3d-viewer")
               (:file "2d-viewer")
               (:file "texture")
               (:file "gl-shader")
               (:file "style")
               (:file "buffer")
               (:file "opengl-object")
               (:file "quad")
               (:file "png-texture")
               (:file "instanced-opengl-object")
               (:file "stl")
               (:file "uniforms"))
  :in-order-to ((test-op (test-op simple-gl.test))))
