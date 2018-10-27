;;;; coalton.lisp

(in-package #:coalton-impl)

(defvar *print-coalton* nil
  "Print Coalton things as in Coalton.")

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

;;; Beware, don't use (VECTOR MONOTYPE)! It will "upgrade" (more like
;;; downgrade, am I right?) to (VECTOR T).
(defun monotype-vector-p (v)
  (and (vectorp v)
       (every (lambda (x) (typep x 'monotype)) v)))

(deftype monotype-vector ()
  '(satisfies monotype-vector-p))

(defstruct (type-application (:constructor type-application (constructor arguments)))
  "A type application."
  (constructor (required 'constructor))
  (arguments   (required 'arguments)   :type monotype-vector :read-only t))

;;; ## Polytypes
;;;
;;; s := t
;;;    | forall a . s  [Quantifier]

(deftype polytype ()
  '(or monotype type-quantifier))

(defstruct (type-quantifier (:constructor type-quantifier (variable expression)))
  "Quantification of a type expression over a type variable."
  (variable   (required 'variable)   :type type-variable :read-only t)
  (expression (required 'expression) :type polytype      :read-only t))

(defun well-formed-type-p (ty)
  "Is the type TY a well-formed type?"
  (labels ((recurse (ty vars)
             (etypecase ty
               ;; We do *NOT* allow implicit universal quantification
               ;; of free variables.
               (type-quantifier
                (recurse (type-quantifier-expression ty)
                         (union vars (list (type-quantifier-variable ty)))))
               (type-variable
                (boolify (member (type-variable-symbol ty) vars)))
               ;; TODO: check arity when we got the data.
               (type-application
                (every (lambda (arg) (recurse arg vars))
                       (type-application-arguments ty))))))
    (recurse ty nil)))

;;; Info Database

(define-global-var **function-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton function definitions.")

(define-global-var **type-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton type definitions.")

