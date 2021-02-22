;;;; freesound.asd

(asdf:defsystem #:freesound
  :description "A client for Freesound.org."
  :author "Nuno Trocado https://nunotrocado.com"
  :license  "MIT"
  :version "1.0"
  :homepage "https://nunotrocado.com/freesound/"
  :source-control (:git "https://github.com/ntrocado/freesound")
  :depends-on ("alexandria" "cl-ppcre" "dexador" "yason" "trivial-open-browser")
  :serial t
  :components ((:file "package")
               (:file "freesound")))
