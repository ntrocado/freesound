;;;; freesound.asd

(asdf:defsystem #:freesound
  :description "A client for Freesound.org"
  :author "Nuno Trocado https://nunotrocado.com"
  :license  "MIT"
  :version "0.0.1"
  :depends-on ("alexandria" "cl-ppcre" "dexador" "yason")
  :serial t
  :components ((:file "package")
               (:file "freesound")))
