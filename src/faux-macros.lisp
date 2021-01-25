;;;; faux-macros.lisp
;;;;
;;;; Some macro defintions that are only valid inside of a COALTON (&
;;;; co.) macro, that aren't actually valid as a toplevel form in
;;;; Lisp.

(in-package #:coalton-impl)

(defun error-coalton-only (name)
  (error "The operator ~S is only valid in a Coalton expression." name))

(defmacro define-coalton-editor-macro (name lambda-list docstring)
  "Define a macro so that Emacs and SLIME see it nicely, and so the forms indent properly. Not intended for actual use in Lisp code."
  (check-type docstring string)
  `(defmacro ,name ,lambda-list
     ,docstring
     (declare (ignore ,@(remove-if (lambda (sym) (char= #\& (char (symbol-name sym) 0)))
                                   lambda-list)))
     (error-coalton-only ',name)))


;;; Top-Level Forms

(define-coalton-editor-macro coalton:define (var &body value)
  "Define VAR to name VALUE. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-type-alias (alias-type real-type)
  "Create an alias for the type REAL-TYPE named ALIAS-TYPE. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-type (name &body definition)
  "Create a new algebraic data type named NAME. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-class (name &body definition)
  "Create a new type class named NAME. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:define-instance (class &body definition)
  "Create an instance of CLASS. (Coalton top-level operator.)")

(define-coalton-editor-macro coalton:declare (var type)
  "Declare the type of a variable. (Coalton top-level operator.)")


;;; Other Constructions

(define-coalton-editor-macro coalton:progn (&body forms)
  "Sequence of left-to-right evaluations.")

(define-coalton-editor-macro coalton:fn (vars &body form)
  "A lambda abstraction.")

(define-coalton-editor-macro coalton:match (expr &body patterns)
  "Pattern matching construct.")

(define-coalton-editor-macro coalton:let (bindings &body form)
  "A lexical LET binding.")

(define-coalton-editor-macro coalton:letrec (bindings &body form)
  "A lexical, recursive LET binding.")

#+#:phase-out-if
(define-coalton-editor-macro coalton:if (test then else)
  "A conditional construct.")

(define-coalton-editor-macro coalton:lisp (type &body lisp-expr)
  "An escape from Coalton into the Lisp world.")
