;; png-texture.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass png-texture ()
  ((file-name :initarg :filename
              :type (or string path))))

(defmethod initialize ((tex texture) &key)
  (with-slots (textures tex-type size) tex
    (when textures
      (error "Initializing texture twice ~a" tex))
    (setf textures (gl:gen-textures 1))
    (bind tex)
    (%gl:tex-storage-2d tex-type 1 :rgba8 (elt size 0) (elt size 1) )
    (fill-texture tex)))

(defmethod reload ((object texture))
  (bind object)
  (with-slots (parameters size tex-type) object
    (dolist (param parameters)
      (gl:tex-parameter tex-type (car param) (cdr param)))
    (fill-texture object)))

(defmethod fill-texture ((object texture))
  (with-slots (size tex-type) object
    (gl:tex-sub-image-2d tex-type
                         0
                         0 0
                         (elt size 0) (elt size 1)
                         :rgba
                         :unsigned-byte
                         #(255 0 0 255))
    (gl:generate-mipmap tex-type)))

(defmethod cleanup ((obj texture))
  "Delete a shader on the GPU."
  (with-slots (textures tex-type) obj
    (when textures
      (gl:bind-texture tex-type 0)
      (gl:delete-textures textures)
      (setf textures nil))))
