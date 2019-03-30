;;;; free-variables.lisp

(in-package #:coalton-impl)

(defun free-variables (value)
  "Compute a list of free variables in the VALUE expression.

NOTE: Just because a variable shows up in the list does *NOT* mean all occurrences of that variable are free in the expression."
  (check-type value node)
  (let ((fv nil))
    (labels ((analyze (expr bv)
               (etypecase expr
                 (node-literal
                  nil)

                 (node-variable
                  (let ((name (node-variable-name expr)))
                    (unless (member name bv)
                      (push name fv))))

                 (node-abstraction
                  (let ((vars (node-abstraction-vars expr)))
                    (analyze (node-abstraction-subexpr expr) (union bv vars))))

                 (node-let
                  ;; Analyze the bindings
                  (mapc (lambda (binding) (analyze (cdr binding) bv))
                        (node-let-bindings expr))
                  ;; Analyze the body
                  (analyze (node-let-subexpr expr)
                           (union bv (mapcar #'car (node-let-bindings expr)))))

                 (node-letrec
                  (let* ((bindings (node-letrec-bindings expr))
                         (subexpr (node-letrec-subexpr expr))
                         (vars (mapcar #'car bindings))
                         (vals (mapcar #'cdr bindings))
                         (bv* (union bv vars)))
                    (mapc (lambda (expr) (analyze expr bv*)) (cons subexpr vals))))

                 (node-if
                  (analyze (node-if-test expr) bv)
                  (analyze (node-if-then expr) bv)
                  (analyze (node-if-else expr) bv))

                 (node-lisp
                  nil)

                 (node-sequence
                  (mapc (lambda (expr) (analyze expr bv)) (node-sequence-exprs expr)))

                 (node-application
                  (let ((rator (node-application-rator expr))
                        (rands (node-application-rands expr)))
                    (mapc (lambda (expr) (analyze expr bv)) (cons rator rands))))

                 (node-match
                  (let ((value (node-match-value expr))
                        (clauses (node-match-clauses expr)))
                    (analyze value bv)
                    (dolist (clause clauses)
                      (analyze (match-clause-value clause)
                               (union bv (ctor-pattern-variables
                                          (match-clause-pattern clause))))))))))
      (analyze value nil)
      fv)))
