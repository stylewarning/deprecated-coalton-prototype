;;;; forward-declarations.lisp

(in-package #:coalton-user)

(coalton
  (declare 1+ (-> integer integer))
  (declare ignore (-> a unit))
  (declare not (-> boolean boolean)))
