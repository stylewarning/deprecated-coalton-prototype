;;;; utilities.lisp

(in-package #:coalton-impl)

(defun required (name)
  (error "The argument/slot ~S is required." name))

(declaim (inline boolify))
(defun boolify (thing)
  (if thing t nil))
