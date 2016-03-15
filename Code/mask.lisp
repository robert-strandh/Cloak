(cl:in-package #:cloak)

(defclass matrix-mask (mask)
  ((%matrix :initarg :matrix :reader matrix)
   (%dx :initarg :dx :reader dx)
   (%dy :initarg :dy :reader dy)))

(defclass computed-mask (mask)
  ((%function :initarg :function :reader function)))
