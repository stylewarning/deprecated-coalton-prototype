;;;; utilities.lisp

(in-package #:coalton-impl)

(defun required (name)
  (error "The argument/slot ~S is required." name))

(declaim (inline boolify))
(defun boolify (thing)
  (if thing t nil))

(defun error-parsing (thing reason-control &rest reason-args)
  (error "Failed to parse ~S because: ~?" thing reason-control reason-args))

(defun error-typing (reason-control &rest reason-args)
  (error "Type error: ~?" reason-control reason-args))

(defun error-type-mismatch (a b)
  (error-typing "The types ~S and ~S don't match." a b))
