(cl:in-package #:cloak)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MATRIX-MASK.

(defclass matrix-mask (mask)
  ((%matrix :initarg :matrix :reader matrix)
   (%dx :initarg :dx :reader dx)
   (%dy :initarg :dy :reader dy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPUTED-MASK.

(defclass computed-mask (mask)
  ((%function :initarg :function :reader function)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TRIANGLE-MASK.

(defclass triangle-mask (mask)
  ((%triangles :initarg :triangles :reader triangles)))
