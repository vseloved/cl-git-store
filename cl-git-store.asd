(in-package #:asdf)

(defsystem #:cl-git-store
  :name "Use git as storage"
  :version "-1"
  :author "Vsevolod Dyomkin <vseloved@gmail.com>"
  :licence "GNU GPL"
  :long-description ""
  :serial t
  :components ((:file "cl-git-store"))
  :depends-on (#:rutils #:cl-ppcre #:cl-fad
               (:feature :sbcl)))
