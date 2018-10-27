;;;; package.lisp

(defpackage #:coalton
  (:documentation "Public interface to COALTON.")
  (:use #:cl)
  (:export))

(defpackage #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:export))
