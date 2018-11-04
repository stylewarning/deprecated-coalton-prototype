;;; hindley-milner.lisp

(in-package #:coalton-impl)

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

(defun set-add (set item)
  (adjoin item set :test 'equalp))

(defun lookup-type (name env non-generic)
  (alexandria:if-let ((var (assoc-find env name)))
    (fresh var non-generic)
    (if (var-knownp name)
        (let ((entry (var-info name)))
          (or (entry-declared-type entry)
              (entry-derived-type entry)
              (error-parsing name "Couldn't determine type of known variable.")))
        (error-parsing name "Undefined symbol"))))

(defun derive-type (value)
  "Derive the type of the Coalton value expressed as a NODE."
  (check-type value node)
  (labels ((analyze (expr env non-generic)
             (setf (node-derived-type expr) (analyze-expr expr env non-generic)))

           (analyze-expr (expr env non-generic)
             (etypecase expr
               (node-literal
                (etypecase (node-literal-value expr)
                  (integer integer-type)))

               (node-variable
                ;; XXX: Check the global environment!!!
                (lookup-type (node-variable-name expr) env non-generic))

               (node-abstraction
                (let* ((var (node-abstraction-var expr))
                       (subexpr (node-abstraction-subexpr expr))
                       (var-ty (make-variable))
                       (ret-ty (analyze subexpr
                                        (assoc-add env var var-ty)
                                        (set-add non-generic var-ty))))
                  (make-function-type var-ty ret-ty)))

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
                (let* ((var (node-letrec-var expr))
                       (val (node-letrec-val expr))
                       (subexpr (node-letrec-subexpr expr))
                       (new-ty (make-variable))
                       (new-env (assoc-add env var new-ty))
                       (defn-ty (analyze val new-env (set-add non-generic new-ty))))
                  (unify new-ty defn-ty)
                  (analyze subexpr new-env non-generic)))

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
                  (assert (= 1 (length rands)))
                  (let* ((fun-ty (analyze rator env non-generic))
                         (arg-ty (analyze (first rands) env non-generic))
                         (ret-ty (make-variable)))
                    (unify (make-function-type arg-ty ret-ty) fun-ty)
                    ret-ty))))))
    (analyze value nil nil)))
