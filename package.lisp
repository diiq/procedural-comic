;;;; package.lisp

(defpackage #:procedural-comic
  (:use #:cl))

(shadow '+ '#:procedural-comic)
(shadow '- '#:procedural-comic)
(shadow '/ '#:procedural-comic)
(shadow '* '#:procedural-comic)
