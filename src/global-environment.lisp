;;;; global-environment.lisp

(in-package #:coalton-impl)

;;;;;;;;;;;;;;;;;;;;;;; Global Value Bindings ;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct entry
  "An entry in the global value database."
  (declared-type nil :type (or null ty cty))
  (derived-type nil :type (or null ty cty))
  (overloaded nil :type boolean)
  source-form
  node)

(define-global-var **global-value-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton global value definitions. This is a map from vars (symbols) to ENTRYs.")

(defun var-knownp (var)
  "Have we seen VAR?"
  (check-type var symbol)
  (nth-value 1 (gethash var **global-value-definitions**)))

(defun var-info (var)
  "What do we know about the known variable VAR?"
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
    (style-warn "Overwriting info entry for ~S" var))
  (setf (gethash var **global-value-definitions**) new-value))

(defun var-overloaded-p (var)
  (entry-overloaded (var-info var)))

(defun (setf var-overloaded-p) (new-value var)
  (setf (entry-overloaded (var-info var)) new-value))


(defun forward-declare-variable (var &optional (declared-type nil declaredp)
                                               (overloaded nil overloaded-provided-p))
  (check-type var symbol)
  (check-type declared-type (or ty cty null))
  (when (var-knownp var)
    (error "Can't forward declare ~S, which is already known." var))
  (setf (gethash var **global-value-definitions**)
        (make-entry))
  (when declaredp
    (setf (var-declared-type var) declared-type))
  (when overloaded-provided-p
    (setf (var-overloaded-p var) overloaded))
  var)

(defun var-declared-type (var)
  (let ((info (var-info var)))
    (entry-declared-type info)))

(defun (setf var-declared-type) (new-value var)
  (check-type new-value (or ty cty))
  (let ((info (var-info var)))
    (alexandria:when-let ((existing-declared-type (entry-declared-type info)))
      (when (type= existing-declared-type new-value)
        (return-from var-declared-type var))
      (style-warn "Overwriting declared type of ~S from ~A to ~A"
                  var
                  (unparse-type existing-declared-type)
                  (unparse-type new-value)))
    (alexandria:when-let ((derived (var-derived-type var)))
      (unless (more-or-equally-specific-type-p derived new-value)
        (error "Cannot declare ~S as ~S because that is ~
                inconsistent with its derived type ~S."
               var
               (unparse-type new-value)
               (unparse-type derived))))
    (setf (entry-declared-type info) new-value)))

(defun var-derived-type (var)
  (let ((info (var-info var)))
    (entry-derived-type info)))

(defun (setf var-derived-type) (new-value var)
  (check-type new-value (or ty cty))
  (let ((info (var-info var)))
    (alexandria:when-let ((existing-derived-type (entry-derived-type info)))
      (when (type= existing-derived-type new-value)
        (return-from var-derived-type var))
      (style-warn "Overwriting derived type of ~S from ~A to ~A"
                  var
                  (unparse-type existing-derived-type)
                  (unparse-type new-value)))
    (alexandria:when-let ((declared (var-declared-type var)))
      (unless (more-or-equally-specific-type-p new-value declared)
        (error "The derived type of ~S, which is ~S, is incompatible ~
                with its previously declared type ~S."
               var
               (unparse-type new-value)
               (unparse-type declared))))
    (setf (entry-derived-type info) new-value)))

;;;;;;;;;;;;;;;;;;;;;; Global Type Definitions ;;;;;;;;;;;;;;;;;;;;;;;

;;; See types.lisp

