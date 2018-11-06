;;;; package.lisp

(defpackage #:coalton
  (:documentation "Public interface to COALTON.")
  (:use)                                ; Keep the package clean!
  (:export
   #:coalton-toplevel
   #:coalton
   #:define
   #:define-type-alias
   #:define-type)
  (:export
   #:->
   #:integer
   #:boolean #:true #:false
   #:unit #:singleton
   )
  (:export
   #:declare
   #:fn
   #:progn
   #:match
   #:let
   #:letrec
   #:if
   #:lisp)
  (:export
   #:type-of))

(defpackage #:coalton-user
  (:documentation "User package for Coalton.")
  (:use #:coalton))

(defpackage #:coalton-impl
  (:documentation "Implementation and runtime for COALTON. This is a package private to the COALTON system and is not intended for public use.")
  (:use #:cl)
  (:import-from #:global-vars
                #:define-global-var
                #:define-global-var*)
  (:import-from #:abstract-classes #:abstract-class #:final-class)
  (:import-from #:singleton-classes #:singleton-class)
  (:export))
