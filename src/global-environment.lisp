;;;; value-environment.lisp

(in-package #:coalton-impl)

;;;;;;;;;;;;;;;;;;;;;;; Global Value Bindings ;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct entry
  internal-name
  declared-type
  derived-type
  source-form
  node)

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

(defun var-definedp (var)
  "Is the var actually defined (as opposed to just declared)?"
  (and (var-knownp var)
       (entry-source-form (var-info var))
       t))

(defun (setf var-info) (new-value var)
  (check-type new-value entry)
  (check-type var symbol)
  (when (var-knownp var)
    (warn "Overwriting info entry for ~S" var))
  (setf (gethash var **global-value-definitions**) new-value))

(defun make-internal-name (s)
  (check-type s symbol)
  (gentemp (symbol-name s) ':coalton-global-symbols))

(defun forward-declare-variable (var &optional declared-type)
  (check-type var symbol)
  (check-type declared-type (or ty null))
  (when (var-knownp var)
    (error "Can't forward declare ~S, which is already known." var))
  (setf (gethash var **global-value-definitions**)
        (make-entry :internal-name (make-internal-name var)))
  (when declared-type
    (setf (var-declared-type var) declared-type))
  var)

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


;;;;;;;;;;;;;;;;;;;;;; Global Type Definitions ;;;;;;;;;;;;;;;;;;;;;;;

;;; See types.lisp

