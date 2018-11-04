;;;; forward-declarations.lisp

(in-package #:coalton-user)

(coalton
  (declare false boolean)
  (declare true boolean)
  (declare 1+ (-> integer integer))
  (declare ignore (-> a unit))
  (declare not (-> boolean boolean)))
