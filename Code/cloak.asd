(cl:in-package #:asdf-user)

(defsystem :cloak
  :depends-on (:acclimation :cl-vectors)
  :serial t
  :components
  ((:file "packages")
   (:file "client-protocol")
   (:file "triangle")
   (:file "mask")))
