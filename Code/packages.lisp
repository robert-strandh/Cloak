(cl:in-package #:common-lisp-user)

(defpackage #:cloak
  (:use #:common-lisp)
  (:shadow #:function)
  (:export #:canvas
           #:mask
           #:matrix-mask
           #:triangle))

(defpackage #:cloak-backend
  (:use #:common-lisp)
  (:export #:pixel))
