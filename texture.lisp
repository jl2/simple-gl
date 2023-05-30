;; texture.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass texture ()
  ((tex-type :initform :texture-2d
             :initarg :type
             :documentation "OpenGL texture type (:texture-2d, :texture-1d, :texture-3d, etc.)")
   (textures :initform nil
             :type (or null list)
             :documentation "OpenGL texture handles.")
   (size :initform #(1 1)
         :initarg :size
         :type sequence
         :documentation "n-dimensional array specifying the size of the texture.")
   (parameters :initform '((:texture-wrap-s . :repeat)
                           (:texture-wrap-t . :repeat)
                           (:texture-base-level . 0)
                           (:texture-max-level . 8)
                           (:texture-min-filter . :linear-mipmap-linear)
                           (:texture-mag-filter . :linear))
               :documentation "OpenGL texture parameters.")))

(defmethod update ((texture texture) elapsed-seconds)
  (declare (ignorable texture elapsed-seconds))
  nil)

(defmethod bind ((object texture))
  (with-slots (tex-type textures) object
    (when textures
      (gl:bind-texture tex-type (car textures)))))

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
    (gl:tex-sub-image-2d tex-type 0
                         0 0
                         (elt size 0) (elt size 1)
                         :rgba :unsigned-byte #(255 0 0 255))
    (gl:generate-mipmap tex-type)))

(defmethod cleanup ((obj texture))
  "Delete a shader on the GPU."
  (with-slots (textures tex-type) obj
    (when textures
      (gl:bind-texture tex-type 0)
      (gl:delete-textures textures)
      (setf textures nil))))
