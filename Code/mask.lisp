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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Turn an instance of TRIANGLE-MASK into an instance of MATRIX-MASK.

;;; Return the extreme points of the triangles in the mask as four
;;; values: MIN-X, MIN-Y, MAX-X, and MAX-Y.
;;;
;;; This function assumes that there is at least one triangle in the
;;; list of triangles in TRIANGLE-MASK.
(defun compute-extremes (triangle-mask)
  (loop for triangle in (triangles triangle-mask)
	minimize (x1 triangle) into min-x
	minimize (x2 triangle) into min-x
	minimize (x3 triangle) into min-x
	maximize (x1 triangle) into max-x
	maximize (x2 triangle) into max-x
	maximize (x3 triangle) into max-x
	minimize (y1 triangle) into min-y
	minimize (y2 triangle) into min-y
	minimize (y3 triangle) into min-y
	maximize (y1 triangle) into max-y
	maximize (y2 triangle) into max-y
	maximize (y3 triangle) into max-y
	finally (return (values min-x min-y max-x max-y))))
