;;; hindley-milner.lisp

(in-package #:coalton-impl)

(defun assoc-find (env name)
  (let ((entry (assoc name env)))
    (and entry
         (cdr entry))))

(defun assoc-add (env name val)
  (labels ((rec (current-assoc new-assoc seen)
             (if (endp current-assoc)
                 (if (null seen)
                     (acons name val (nreverse new-assoc))
                     (nreverse new-assoc))
                 (let ((entry (first current-assoc)))
                   (if (eq name (car entry))
                       (rec (rest current-assoc) (acons name val new-assoc) t)
                       (rec (rest current-assoc) (cons entry new-assoc) nil))))))
    (rec env nil nil)))

(defun assoc-add* (env names vals)
  (loop :for name :in names
        :for val :in vals
        :do (setf env (assoc-add env name val))
        :finally (return env)))

(defun set-add (set item)
  (adjoin item set :test 'equalp))

(defun set-add* (set items)
  (dolist (item items set)
    (setf set (set-add set item))))

(defun error-unknown-symbol (name)
  (error "Undefined symbol: ~S" name))

(defun lookup-type (name env non-generic &key (continue 'error-unknown-symbol))
  (alexandria:if-let ((var (assoc-find env name)))
    (fresh var non-generic)
    (if (var-knownp name)
        (let ((entry (var-info name)))
          (fresh (or (entry-declared-type entry)
                     (entry-derived-type entry)
                     (error "Couldn't determine type of known variable ~S." name))))
        (funcall continue name))))

(defun derive-type (value &aux constraints)
  "Derive the type of the Coalton value expressed as a NODE."
  (check-type value node)
  (labels ((add-cxs (cxs)
             (setf CONSTRAINTS (append CONSTRAINTS cxs)))

           (analyze (expr env non-generic)
             (setf (node-derived-type expr) (analyze-expr expr env non-generic)))

           (analyze-expr (expr env non-generic)
             (etypecase expr
               (node-literal
                (etypecase (node-literal-value expr)
                  (integer integer-type)
                  (string  string-type)))

               (node-variable
                ;; The global environment is checked as a part of
                ;; LOOKUP-TYPE.
                (let ((ty
                        (lookup-type (node-variable-name expr) env non-generic)))
                  (etypecase ty
                    (ty ty)
                    (cty
                     (add-cxs (cty-constraints ty))
                     (cty-expr ty)))))

               (node-abstraction
                (let* ((vars (node-abstraction-vars expr))
                       (arity (length vars))
                       (subexpr (node-abstraction-subexpr expr))
                       (var-tys (loop :repeat arity :collect (make-variable)))
                       (ret-ty (analyze subexpr
                                        (assoc-add* env vars var-tys)
                                        (set-add* non-generic var-tys))))
                  (make-function-type var-tys ret-ty)))

               (node-let
                ;; Won't deal with dupe variables.
                (let* ((bindings (node-let-bindings expr))
                       (subexpr (node-let-subexpr expr))
                       (vars (mapcar #'car bindings))
                       (vals (mapcar #'cdr bindings))
                       (val-tys (mapcar (lambda (val) (analyze val env non-generic)) vals)))
                  ;; Build up the new type environment; just mutate the
                  ;; existing ENV binding.
                  (loop :for var :in vars
                        :for val-ty :in val-tys
                        :do (setf env (assoc-add env var val-ty)))
                  (analyze subexpr env non-generic)))

               (node-letrec
                (let* ((bindings (node-letrec-bindings expr))
                       (subexpr (node-letrec-subexpr expr))
                       (vars (mapcar #'car bindings))
                       (tyvars (loop :repeat (length bindings) :collect (make-variable)))
                       (gamma* (loop :with gamma := env
                                     :for var :in vars
                                     :for tyvar :in tyvars
                                     :do (setf gamma (assoc-add gamma var tyvar))
                                     :finally (return gamma)))
                       (ng (loop :with ng := non-generic
                                 :for tyvar :in tyvars
                                 :do (setf ng (set-add ng tyvar))
                                 :finally (return ng)))
                       (vals (mapcar #'cdr bindings))
                       (defn-tys (loop :for val :in vals
                                       :collect (analyze val gamma* ng))))
                  (loop :for tyvar :in tyvars
                        :for defn-ty :in defn-tys
                        :do (unify tyvar defn-ty))
                  (analyze subexpr gamma* non-generic))
                #+#:single-variable ; (LETREC VAR VAL SUBEXPR)
                (let* ((var (node-letrec-var expr))
                       (val (node-letrec-val expr))
                       (subexpr (node-letrec-subexpr expr))
                       (new-ty (make-variable))
                       (new-env (assoc-add env var new-ty))
                       (defn-ty (analyze val new-env (set-add non-generic new-ty))))
                  (unify new-ty defn-ty)
                  (analyze subexpr new-env non-generic)))

               #+#:phase-out-if
               (node-if
                (let ((test-ty (analyze (node-if-test expr) env non-generic)))
                  (unify boolean-type test-ty)
                  (let ((then-ty (analyze (node-if-then expr) env non-generic))
                        (else-ty (analyze (node-if-else expr) env non-generic)))
                    (unify then-ty else-ty)
                    then-ty)))

               (node-lisp
                ;; Return the stated type at face value.
                (node-lisp-type expr))

               (node-sequence
                (let ((exprs (node-sequence-exprs expr)))
                  (cond
                    ((endp exprs)
                     unit-type)
                    ((endp (rest exprs))
                     (analyze (first exprs) env non-generic))
                    (t
                     (let ((middle-exprs (butlast exprs))
                           (last-expr (first (last exprs))))
                       ;; Derive the types of all of the middle
                       ;; expressions, ensuring they unify with UNIT.
                       (loop :for mid-expr :in middle-exprs
                             :for ty := (analyze mid-expr env non-generic)
                             :do (unify unit-type ty))
                       ;; Derive the type of the last expression.
                       (analyze last-expr env non-generic))))))

               (node-application
                (let ((rator (node-application-rator expr))
                      (rands (node-application-rands expr)))
                  (let ((fun-ty  (analyze rator env non-generic))
                        (arg-tys (loop :for rand :in rands
                                       :collect (analyze rand env non-generic)))
                        (ret-ty  (make-variable)))
                    (unify (make-function-type arg-tys ret-ty) fun-ty)
                    ret-ty)))

               (node-match
                (let* ((value (node-match-value expr))
                       (clauses (node-match-clauses expr))
                       (expected-tycon (node-match-tycon expr))
                       (expected-var-tys (loop :repeat (tycon-arity expected-tycon)
                                               :collect (make-variable)))
                       (expected-value-ty
                         (apply #'tyapp expected-tycon expected-var-tys))
                       (value-ty (analyze value env non-generic)))
                  ;; Go through each of the clauses, create fresh
                  ;; variables for the binding sites, and unify with
                  ;; those.
                  (let ((result-ty nil))
                    (dolist (clause clauses)
                      (let ((pat (match-clause-pattern clause))
                            (val (match-clause-value clause)))
                        (etypecase pat
                          (ctor-pattern
                           (let* ((vars (ctor-pattern-variables pat))
                                  (pat-ty (var-declared-type (ctor-pattern-ctor pat)))
                                  (var-tys (loop :repeat (length vars)
                                                 :collect (make-variable)))
                                  (res-ty (analyze val
                                                   (assoc-add* env vars var-tys)
                                                   (set-add* non-generic var-tys))))
                             ;; Unify with previous clause values.
                             (cond
                               ((null result-ty)
                                (setf result-ty res-ty))
                               (t
                                (unify result-ty res-ty)))
                             ;; Now unify what we know is our declared
                             ;; type for the pattern with what we
                             ;; learned about the variables.
                             (unless (endp vars)
                               (unify pat-ty
                                      (make-function-type var-tys expected-value-ty))))))))
                    ;; Unify our value's type with what we expect
                    ;; simply from the constructors
                    ;; used. EXPECTED-VALUE-TY will have accumulated
                    ;; unifications.
                    (unify value-ty expected-value-ty)
                    ;; Return the type of the entire expression.
                    result-ty))))))
    (let ((ty (analyze value nil nil)))
      (if (null CONSTRAINTS)
          ty
          (make-cty ty :constraints CONSTRAINTS)))))
