;;;; package.lisp

(defpackage #:coalton
  (:documentation "Public interface to COALTON.")
  (:use)                                ; Keep the package clean!
  (:export
   #:coalton
   #:define
   #:define-type-alias
   #:define-type)
  (:export
   #:->
   )
  (:export
   #:forall
   #:declare
   #:fn
   #:progn
   #:match))

(defpackage #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:import-from #:global-vars #:define-global-var)
  (:export))
