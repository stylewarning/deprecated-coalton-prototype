;;;; coalton.lisp

(in-package #:coalton-impl)

;;; # Hindley-Milner Types
;;;
;;; ## Monotypes
;;;
;;; t := a           [Variable]
;;;    | C t ... t   [Application]

(deftype monotype ()
  '(or type-variable type-application))

(defstruct (type-variable (:constructor type-variable (symbol)))
  "A bare type variable. Required to be within a quantifier."
  (symbol (required 'symbol) :type symbol :read-only t))

(defstruct (type-application (:constructor type-application (constructor arguments)))
  "A type application."
  (constructor (required 'constructor))
  (arguments (required 'arguments) :type (vector monotype) :read-only t))

;;; ## Polytypes
;;;
;;; s := t
;;;    | forall a . s  [Quantifier]

(deftype polytype ()
  '(or monotype type-quantifier))

(defstruct (type-quantifier (:constructor type-quantifier (variable expression)))
  "Quantification of a type expression over a type variable."
  (variable (required 'variable) :type type-variable :read-only t)
  (expression (required 'expression) :type polytype :read-only t))

;;;

(define-global-var **function-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton function definitions.")

(define-global-var **type-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton type definitions.")

