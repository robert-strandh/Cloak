(cl:in-package #:cloak)

(defclass standard-triangle (triangle)
  ((%x1 :initarg :x1 :reader x1)
   (%y1 :initarg :y1 :reader y1)
   (%x2 :initarg :x2 :reader x2)
   (%y2 :initarg :y2 :reader y2)
   (%x3 :initarg :x3 :reader x3)
   (%y3 :initarg :y3 :reader y3)))
