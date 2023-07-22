;; quad.lisp
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

(in-package :simple-gl)

(defclass quad-positions (attribute-buffer)
  ((attributes :initform '(("in_color" . :vec4))
               :initarg :attributes))
  (:documentation "A quad."))


(defmethod refill-buffers ((object quad-positions) &key)
  (when (buffers object)
    (error "Object buffers already setup!"))

  ;; (set-buffer object
  ;;             :vertices
  ;;             (constant-attribute-buffer

  ;;              `(
  ;;                ,(vec3 -1.0f0 -1.0f0 0.0f0)
  ;;                ,(vec2 0.0f0 0.0f0)

  ;;                ,(vec3 1.0f0 -1.0f0 0.0f0)
  ;;                ,(vec2 1.0f0 0.0f0)

  ;;                ,(vec3 1.0f0  1.0f0 0.0f0)
  ;;                ,(vec2 1.0f0 1.0f0)

  ;;                ,(vec3 -1.0f0 1.0f0 0.0f0)
  ;;                ,(vec2 0.0f0 1.0f0))
  ;;              (* 4 5)
  ;;              '(("in_position" . :vec3)
  ;;                ("in_tex" . :vec2))))

  ;; (set-buffer object
  ;;             :indices
  ;;             (make-instance 'index-buffer
  ;;                            :idx-count 6
  ;;                            :pointer (to-gl-array :unsigned-int
  ;;                                                  6
  ;;                                                  #(0 1 2 2 3 0))))
  )
