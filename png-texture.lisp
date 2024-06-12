;; png-texture.lisp
;;
;; Copyright (c) 2023 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

(defclass png-texture (texture)
  ((file-name :initarg :filename
              :type (or string path))
   (png-data :initform nil)))

(defmethod initialize ((tex png-texture) &key)
  (format t "Initializing texture!~%")
  (with-slots (textures tex-type size png-data file-name) tex
    (when textures
      (error "Initializing texture twice ~a" tex))

    (setf textures (gl:gen-textures 1))
    (bind tex)
    (setf png-data (simple-png:read-png file-name))
    (setf size (subseq (array-dimensions png-data) 0 2))

    (%gl:tex-storage-2d tex-type 1 (if (= 4 (array-dimension png-data 2))
                                       :rgba8
                                       :rgb8)
                        (array-dimension png-data 0)
                        (array-dimension png-data 1) )
    (fill-texture tex)))


(defmethod fill-texture ((object png-texture))
  (with-slots (size tex-type png-data file-name) object
    (when (null png-data)
      (setf png-data (simple-png:read-png file-name))
      (when (null png-data)
        (error "Could not read ~a" file-name))
      (setf size (subseq (array-dimensions png-data) 0 2)))

    (when (not (null png-data))
      (gl:tex-sub-image-2d tex-type 0
                           0 0
                           (array-dimension png-data 0)
                           (array-dimension png-data 1)
                           (if (= 3 (array-dimension png-data 2))
                               :rgb
                               :rgba)
                           :unsigned-byte
                           (make-array (apply #'* (array-dimensions png-data))
                                       :element-type '(unsigned-byte 8)
                                       :displaced-to png-data))
      (gl:generate-mipmap tex-type))))

(defun png-quad (file-name)
  (make-instance 'sgl:quad
                 :styles (list (cons :blt
                                     (sgl:make-style-from-files "texture.frag" "texture.vert")))
                 :textures (list (make-instance 'sgl:png-texture  :filename file-name))))
