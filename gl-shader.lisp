;; gl-shader.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

;; This library depends on a number of conventions to automatically handle shaders.
;; 1. Shader file names must specify the shader type as part of the file name.
;;    For example, 'plastic-fragment.glsl', 'plastic-vertex.glsl'
;; 2. Layout information is 'parsed' out of the shader using regular expressions.
;;    They work for the shader's I write, but may need improvements or replacements.

(defparameter *shader-dirs* (list (asdf:system-relative-pathname :simple-gl "shaders/"))
  "Directory containing simple-gl shaders.")

(defun simple-gl-shader (fname)
  (loop
    :for path :in *shader-dirs*
    :until (probe-file (merge-pathnames fname path))
    :finally (return (merge-pathnames fname path))))

;; Shader
(define-condition shader-error (error)
  ((status :initarg :status :reader shader-compile-status)
   (object :initarg :object :reader shader-object)
   (info-log :initarg :info-log :reader shader-compile-info-log)))

(defmethod print-object ((shader-error shader-error) stream)
  (with-slots (status object info-log) shader-error
    (format stream "OpenGL Compiler Error: ~a~%~%~a~%~%Info log:~%==========~%~a" status (get-source object) info-log)))

(defclass gl-shader ()
  ((shader :initform 0 :type fixnum :accessor shader)
   (shader-type :initarg :shader-type))
  (:documentation "An opengl shader class."))


(defgeneric get-source (shader)
  (:documentation "Return shader source code as a string."))

(defgeneric compile-shader (shader)
  (:documentation "Read source from source file and compile shader"))

(defclass gl-file-shader (gl-shader)
  ((source-file :initarg :source-file :type (or pathname string) :accessor source-file))
  (:documentation "An OpenGL shader whose source code is stored in a file."))

(defmethod print-object ((object gl-file-shader) stream)
  (with-slots (source-file) object
    (format stream "(make-instance 'sgl:gl-file-shader ~a)" source-file)))

;; (defmethod print-object ((shader gl-shader) stream)
;;   (format stream "(make-instance 'simple-gl:gl-file-shader :source-file ~s )" (source-file shader)))

(defparameter *glsl-type-db*
  ;; gl-type, component type,  component count, byte size, vec4 sizae
  `(
    (:float   :float   1  ,(cffi:foreign-type-size :float) 1)
    (:int     :int     1  ,(cffi:foreign-type-size :int) 1)
    (:double  :double  1  ,(cffi:foreign-type-size :double) 1)

    (:vec3    :float   3  ,(* 3 (cffi:foreign-type-size :float)) 1)
    (:vec4    :float   4  ,(* 4 (cffi:foreign-type-size :float)) 1)
    (:vec2    :float   2  ,(* 2 (cffi:foreign-type-size :float)) 1)

    (:mat4    :float   16  ,(* 4 4 (cffi:foreign-type-size :float)) 4)
    (:mat3    :float   9  ,(* 3 3 (cffi:foreign-type-size :float)) 4)

    (:dvec3   :double  3  ,(* 3 (cffi:foreign-type-size :double)) 2)
    (:dvec4   :double  4  ,(* 4 (cffi:foreign-type-size :double)) 2)
    (:dvec2   :double  2  ,(* 2 (cffi:foreign-type-size :double)) 2)

    (:dmat4   :double 4  ,(* 4 4 (cffi:foreign-type-size :double)) 8)
    (:dmat3   :double  9  ,(* 3 3 (cffi:foreign-type-size :double)) 8)

    ))

(defun glsl-byte-size (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (cadddr val)))

(defun glsl-component-count (tname)
  (when-let ((val (assoc tname
                         *glsl-type-db*)))
    (caddr val)))

(defun glsl-type-info (tname)
  (cdr (assoc tname *glsl-type-db*)))


(defun lookup-shader-type (file-name)
  (let ((pn (pathname-name file-name)))
    (cond ((ends-with-subseq "-vertex" pn)
           :vertex-shader)
          ((ends-with-subseq "-fragment" pn)
           :fragment-shader)
          ((ends-with-subseq "-compute" pn)
           :compute-shader)
          ((ends-with-subseq "-geometry" pn)
           :geometry-shader)
          ((ends-with-subseq "-tess-eval" pn)
           :tess-evaluation-shader)
          ((ends-with-subseq "-tess-control" pn)
           :tess-control-shader))))


(defun read-shader (file-name &optional type)
  "Read a shader language file and parse out basic information, like type and layout"
  (let ((stype (if type type (lookup-shader-type file-name)))
        (real-name (if (uiop:file-exists-p file-name)
                       file-name
                       (simple-gl-shader file-name))))
    (when (not (uiop:file-exists-p real-name))
      (error "~a ~a do not exist!" file-name real-name))
    (make-instance 'gl-file-shader :source-file real-name :shader-type stype)))

(defmethod get-source ((shader gl-shader))
  "")

(defmethod get-source ((shader gl-file-shader))
  (with-slots (source-file) shader
    (read-file-into-string source-file)))

(defmethod initialize ((shader gl-shader) &key)
  (compile-shader shader))

(defmethod cleanup ((shade gl-shader))
  (with-slots (shader) shade
    (when (> shader 0)
      (setf shader 0))))

(define-condition shader-compile-error (shader-error) ())

(defmethod compile-shader ((shadr gl-shader))
  (with-slots (shader shader-type) shadr
    (when (zerop shader)
      (setf shader (gl:create-shader shader-type)))
    (gl:shader-source shader (get-source shadr))
    (gl:compile-shader shader)
    (let ((compile-result (gl:get-shader shader :compile-status)))
      (when (not (eq t compile-result))
        (error 'shader-compile-error
               :status compile-result
               :object shadr
               :info-log (gl:get-shader-info-log shader))))))
