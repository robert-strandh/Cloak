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

;;; Given three vertices of a triangle that are known to be in
;;; clockwise order, render the triangle to the matrix.
;;;
;;; The clockwise order is defined as the AA library defines it.
(defun render-clockwise-vertices (x1 y1 x2 y2 x3 y3 matrix dx dy)
  (let ((state (net.tuxee.aa:make-state)))
    (net.tuxee.aa:line-f state x1 y1 x2 y2)
    (net.tuxee.aa:line-f state x2 y2 x3 y3)
    (net.tuxee.aa:line-f state x3 y3 x1 y1)
    (flet ((add-opacity (x y opacity)
	     (incf (aref matrix (- y dy) (- x dx))
		   (/ opacity 256d0))))
      (net.tuxee.aa:cells-sweep state #'add-opacity))))

;;; A line is defined by two points <X1,Y1> and <X2,Y2>.  A point is
;;; defined by <X,Y>.  This function returns a true value if the point
;;; lies on one side of the line and false if the point lies on the
;;; other side of the line.  It is assumed that the point does not lie
;;; on the line.
(defun point-side-of-line (x1 y1 x2 y2 x y)
  (minusp (- (* (- x x1) (- y2 y1)) (* (- y y1) (- x2 x1)))))

(defun render-vertices (x1 y1 x2 y2 x3 y3 matrix dx dy)
  (if (point-side-of-line x1 y1 x2 y2 x3 y3)
      (render-clockwise-vertices x1 y1 x3 y3 x2 y2 matrix dx dy)
      (render-clockwise-vertices x1 y1 x2 y2 x3 y3 matrix dx dy)))

(defun render-triangle (triangle matrix dx dy)
  (render-vertices (x1 triangle)
		   (y1 triangle)
		   (x2 triangle)
		   (y2 triangle)
		   (x3 triangle)
		   (y3 triangle)
		   matrix
		   dx
		   dy))
		   
