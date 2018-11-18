;;;; toplevel-declare.lisp

(in-package #:coalton-impl)

;;; Handling of COALTON:DECLARE at the top-level.

(defun check-compound-form (form starts-with)
  "Check that FORM is a compound form starting with STARTS-WITH."
  (unless (and (not (atom form))
               (eql starts-with (first form)))
    (error-parsing form "The form is expected to be compound starting with ~S" starts-with)))

(defun check-compound-form-length (form from &optional (to from))
  "Check that FORM is of length between FROM and TO inclusive. If TO is NIL (default: FROM), then the length can be unbounded."
  (check-type from integer)
  (check-type to (or null integer))
  (unless (if (null to)
              (<= from (length form))
              (<= from (length form) to))
    (error-parsing form "The form is expected to have length between ~D and ~
                         ~:[infinity~;~:*~D~] inclusive."
                   from
                   to)))

(defun parse-declare-form (form)
  "Parse a COALTON:DECLARE form."
  (check-compound-form form 'coalton:declare)
  (check-compound-form-length form 3)
  (destructuring-bind (declare-symbol var type-expr) form
    (declare (ignore declare-symbol))
    (unless (symbolp var)
      (error-parsing form "The second argument should be a symbol."))
    (values var (parse-type-expression type-expr))))

(defun process-toplevel-declarations (decl-forms)
  (dolist (form decl-forms)
    (multiple-value-bind (var type) (parse-declare-form form)
      ;; This just has compile-time effects. It doesn't produce
      ;; executable code.
      (unless (var-knownp var)
        (forward-declare-variable var))
      (setf (var-declared-type var) type)))
  ;; Produce no code.
  nil)
