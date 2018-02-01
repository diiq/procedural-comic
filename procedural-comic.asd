;;;; procedural-comic.asd

(asdf:defsystem #:procedural-comic
  :description "Makes a comic book, procedurally"
  :author "Sam Bleckley <sam@climatum.com>"
  :license  "CC 0"
  :version "0.0.1"
  :serial t
  :depends-on (#:vecto)
  :components ((:file "package")
               (:file "point")
               (:file "string-image")
               (:file "vecto-image")
               (:file "panelling")
               (:file "procedural-comic")))
