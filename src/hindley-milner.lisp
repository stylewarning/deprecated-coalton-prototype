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

(defun derive-type (form)
  "Derive the type of the Coalton value expression FORM."
  (labels ((analyze (expr env non-generic)
             (cond
               ((atom expr)
                (etypecase expr
                  (null    (error-parsing expr "NIL is not allowed!"))
                  (symbol  (analyze-variable expr env non-generic))
                  (integer integer-type)))
               ((alexandria:proper-list-p expr)
                (alexandria:destructuring-case expr
                  ((coalton:fn var subexpr)
                   (analyze-abstraction var subexpr env non-generic))
                  ((coalton:let bindings subexpr)
                   (analyze-let bindings subexpr env non-generic))
                  ((coalton:if test then else)
                   (analyze-if test then else env non-generic))
                  ((coalton:lisp type lisp-expr)
                   (analyze-lisp type lisp-expr env non-generic))
                  ((coalton:match expr &rest patterns)
                   (analyze-match expr patterns env non-generic))
                  ((coalton:progn &rest exprs)
                   (analyze-sequence exprs env non-generic))
                  ((t &rest rands)
                   (analyze-application (first expr) rands env non-generic))))
               (t (error-parsing expr "The expression is not a valid value expression."))))

           #+ignore
           (analyze-atom (atom env)
             (declare (ignore env))
             ;; Just return the atom.
             atom)

           (analyze-variable (var env non-generic)
             ;; XXX: Check the global environment!!!
             (lookup-type var env non-generic))

           (analyze-abstraction (var subexpr env non-generic)
             (let* ((var-ty (make-variable))
                    (ret-ty (analyze subexpr
                                     (assoc-add env var var-ty)
                                     (set-add non-generic var-ty))))
               (make-function-type var-ty ret-ty)))

           (analyze-let (bindings subexpr env non-generic)
             ;; Won't deal with dupe variables.
             (let* ((vars (mapcar #'first bindings))
                    (vals (mapcar #'second bindings))
                    (val-tys (mapcar (lambda (val) (analyze val env non-generic)) vals)))
               ;; Build up the new type environment; just mutate the
               ;; existing ENV binding.
               (loop :for var :in vars
                     :for val-ty :in val-tys
                     :do (setf env (assoc-add env var val-ty)))
               (analyze subexpr env non-generic)))
           
           ;; TODO: LETREC!
           #+ignore
           (analyze-letrec (v defn body env non-generic)
             (let* ((new-ty (make-variable))
                    (new-env (assoc-add env v new-ty))
                    (defn-ty (rec defn new-env (set-add non-generic new-ty))))
               (unify new-ty defn-ty)
               (rec body new-env non-generic)))

           (analyze-if (test then else env non-generic)
             (let ((test-ty (analyze test env non-generic)))
               (unify boolean-type test-ty)
               (let ((then-ty (analyze then env non-generic))
                     (else-ty (analyze else env non-generic)))
                 (unify then-ty else-ty)
                 then-ty)))

           (analyze-lisp (type lisp-expr env non-generic)
             (declare (ignore lisp-expr env non-generic))
             (parse-type-expression type))

           (analyze-match (expr patterns env non-generic)
             (declare (ignore expr patterns env non-generic))
             (error "can't type derive a MATCH!"))

           (analyze-sequence (exprs env non-generic)
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
                  (analyze last-expr env non-generic)))))

           (analyze-application (rator rands env non-generic)
             (assert (= 1 (length rands)))
             (let* ((fun-ty (analyze rator env non-generic))
                    (arg-ty (analyze (first rands) env non-generic))
                    (ret-ty (make-variable)))
               (unify (make-function-type arg-ty ret-ty) fun-ty)
               ret-ty)))
    (analyze form nil nil)))
