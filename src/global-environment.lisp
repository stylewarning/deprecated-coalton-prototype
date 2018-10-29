;;;; value-environment.lisp

(in-package #:coalton-impl)

(defstruct entry
  declared-type
  derived-type
  source-form)

(define-global-var **global-value-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton global value definitions.")

(defun var-knownp (var)
  (check-type var symbol)
  (nth-value 1 (gethash var **global-value-definitions**)))

(defun var-info (var)
  (check-type var symbol)
  (multiple-value-bind (val exists?) (gethash var **global-value-definitions**)
    (unless exists?
      (error "Could not retrieve the type of ~S because it is unknown." var))
    val))

(defun (setf var-info) (new-value var)
  (check-type new-value entry)
  (check-type var symbol)
  (when (var-knownp var)
    (warn "Overwriting info entry for ~S" var))
  (setf (gethash var **global-value-definitions**) new-value))

(defun forward-declare-variable (var)
  (check-type var symbol)
  (when (var-knownp var)
    (error "Can't forward declare ~S, which is already known." var))
  (setf (gethash var **global-value-definitions**) (make-entry)))

(defun var-declared-type (var)
  (let ((info (var-info var)))
    (entry-declared-type info)))

(defun (setf var-declared-type) (new-value var)
  (let ((info (var-info var)))
    (when (entry-declared-type info)
      (warn "Overwriting declared type of ~S" var))
    (setf (entry-declared-type info) new-value)))

(defun var-derived-type (var)
  (let ((info (var-info var)))
    (entry-derived-type info)))

(defun (setf var-derived-type) (new-value var)
  (let ((info (var-info var)))
    (when (entry-derived-type info)
      (warn "Overwriting derived type of ~S" var))
    (setf (entry-derived-type info) new-value)))

