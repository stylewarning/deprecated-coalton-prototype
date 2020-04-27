;;;; package.lisp

(in-package #:cl-user)

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
   #:*
   #:void
   #:integer
   #:string
   #:boolean #:true #:false
   #:unit)
  (:export
   #:declare
   #:fn
   #:progn
   #:match
   #:let
   #:letrec
   #:if
   #:cond
   #:else                               ; Syntax for COND
   #:lisp
   #:match)
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
  (:export
   #:coalton-parse-error                ; CONDITION
   #:coalton-parse-error-form           ; READER
   #:coalton-parse-error-reason-control ; READER
   #:coalton-parse-error-reason-args    ; READER
   #:coalton-type-error                 ; CONDITION
   #:unification-error                  ; CONDITION
   #:unification-error-first-type       ; READER
   #:unification-error-second-type      ; READER
   #:type-mismatch                      ; CONDITION
   #:type-mismatch-types                ; READER
   #:arity-mismatch                     ; CONDITION
   #:arity-mismatch-arities             ; READER
   #:tuple-size-mismatch                ; CONDITION
   #:tuple-size-mismatch-sizes          ; READER
   #:non-terminating-unification-error  ; CONDITION
   #:non-terminating-unification-error-contained-type
                                        ; READER
   #:non-terminating-unification-error-containing-type
                                        ; READER
   ))

(defpackage #:coalton-global-symbols
  (:documentation "A place that global value names are stashed. We don't use uninterned symbols so that they can be reified through the compilation process.")
  (:use))
