;; uniforms.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>

(in-package #:simple-gl)

;; TODO: Consider creating float-uniform, mat4-uniform, etc. subclasses that call the
;; correct gl:uniform* functions, and remove the big cond in use-uniform.
(defclass uniform ()
  ((name :initarg :name
         :type string
         :documentation "Uniform variable name.")
   (type :initarg :type
         :documentation "Uniform variable type.")
   (value :initarg :value
          :initform t
          :type t
          :documentation "Current uniform value.")
   (modified :initform t
             :documentation "Whether the uniform's current value has been sent to OpenGL."))
  (:documentation "A uniform variable parameter to a shader."))


(defmethod show-info ((uniform uniform) &key (indent 0))
  (let ((this-ws (indent-whitespace indent)))
    (with-slots (name type value modified) uniform
      (format t "~a~a ~a = ~a ~a~%" this-ws type name value (if modified "modified" "not modified")))))

(defmethod print-object ((object uniform) stream)
  (with-slots (name type value modified) object
    (format stream "~a ~a = ~a (~a)~%" type name value (if modified "modified" "not modified"))))

(defmethod update ((uniform uniform) elapsed-seconds)
  (declare (ignorable uniform elapsed-seconds))
  nil)

(defmethod use-uniform ((uniform uniform) program)
  "Bind the uniform's value in the program."

  (with-slots (name type value modified) uniform
    (when modified
      (let ((location (gl:get-uniform-location program name)))

        ;; Only assign values to uniforms that are used by the program
        (when (and modified (>= location 0))
          (cond (
                 (eq :mat4 type)
                 (gl:program-uniform-matrix program location
                                            4
                                            (vector (marr4 value))
                                            t))

                ((eq :mat3 type)
                 (gl:program-uniform-matrix program location
                                            3
                                            (vector (marr3 value))
                                            t))
                ((eq :dmat4 type)
                 (gl:program-uniform-matrix program location
                                            4
                                            (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr4 value)))
                                            t))

                ((eq :dmat3 type)
                 (gl:program-uniform-matrix program location
                                            3
                                            (map 'vector (lambda (v) (coerce v 'double-float)) (vector (marr3 value)))
                                            t))

                ((eq :int type)
                 (gl:program-uniformi program location
                                      value))

                ((eq :float type)
                 (gl:program-uniformf program location
                                      value))

                ((eq :vec2 type)
                 (gl:program-uniformf program location
                                      (vx value)
                                      (vy value)))

                ((eq :vec3 type)
                 (gl:program-uniformf program location
                                      (vx value)
                                      (vy value)
                                      (vz value)))

                ((eq :vec4 type)
                 (gl:program-uniformf program location
                                      (vx value)
                                      (vy value)
                                      (vz value)
                                      (vw value)))
                (t
                 (error "Don't know how to set type ~a" type)))
          (setf modified nil))))))

(defmethod set-value (uniform new-value &optional (new-type nil))
  (with-slots (value modified type) uniform
    (setf value new-value)
    (when new-type
      (setf type new-type))
    (setf modified t)))

(defmethod get-value (uniform)
  (with-slots (value modified type) uniform
    value))

(defmethod cleanup ((uniform uniform))
  t)
