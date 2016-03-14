(cl:in-package #:asdf-user)

(defsystem :cloak
  :depends-on (:acclimation)
  :serial t
  :components
  ((:file "packages")
   (:file "client-protocol")))
