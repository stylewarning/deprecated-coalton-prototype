;;;; type-errors.lisp

(in-package #:coalton-impl)

;;; This file contains various type-related errors. We put them here
;;; for ease of reference, even though the errors themselves are
;;; primarily used in other files.

(define-condition coalton-type-error (error)
  ()
  (:documentation "A type error occuring in Coalton."))

(define-condition unification-error (coalton-type-error)
  ((first-type :initarg :first-type
               :reader unification-error-first-type)
   (second-type :initarg :second-type
                :reader unification-error-second-type))
  (:documentation "A general error indicating unification failure."))

(define-condition type-mismatch (unification-error)
  ((mismatched-types :initarg :mismatched-types
                     :reader type-mismatch-types))
  (:documentation "An error that is signalled when unification fails due to a type mismatch.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S are incompatible specifically ~
                          because ~S and ~S do not unify."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (first (type-mismatch-types c)))
                       (unparse-type (second (type-mismatch-types c))))))))

(define-condition arity-mismatch (type-mismatch)
  ((mismatched-arities :initarg :mismatched-arities
                       :reader arity-mismatch-arities))
  (:documentation "An error that is signalled when unification fails due to an arity mismatch.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S are incompatible specifically ~
                          because the function types ~S and ~S have different ~
                          arities (~D and ~D respectively)."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (first (type-mismatch-types c)))
                       (unparse-type (second (type-mismatch-types c)))
                       (first (arity-mismatch-arities c))
                       (second (arity-mismatch-arities c)))))))

(define-condition non-terminating-unification-error (unification-error)
  ((contained-type :initarg :contained-type
                   :reader non-terminating-unification-error-contained-type)
   (containing-type :initarg :containing-type
                    :reader non-terminating-unification-error-containing-type))
  (:documentation "An error that is signalled when unification would not terminate due to infinite recursion.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S cannot be unified because ~
                          it will cause infinite unification. Specifically,
                          ~S occurs in ~S."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (non-terminating-unification-error-contained-type c))
                       (unparse-type (non-terminating-unification-error-containing-type c)))))))
