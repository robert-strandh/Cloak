(cl:in-package #:common-lisp-user)

(defpackage #:cloak
  (:use #:common-lisp)
  (:export #:canvas))

(defpackage #:cloak-backend
  (:use #:common-lisp)
  (:export #:pixel))
