(cl:in-package #:cloak)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MATRIX-MASK.
;;;
;;; The MATRIX slot contains a 2-dimensional array of DOUBLE-FLOAT
;;; opacity values.  Typically, there will be no row and no column in
;;; that array with all opacity values being zero, but that is not a
;;; requirement.
;;;
;;; The slots DX and DY determine the position of the array in the
;;; plane.  The opacity value of the element determined by the form
;;; (AREF MATRIX Y X) should be interpreted as the opacity value of
;;; the pixel at position <(+ X DX),(+ Y DY)>.

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
