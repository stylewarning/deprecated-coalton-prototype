;;;; node.lisp

;;;; A representation of a value.

(in-package #:coalton-impl)

;;; Types are fully defined in types.lisp
(defstruct (ty (:constructor nil)))

(defun type-list-p (thing)
  (and (alexandria:proper-list-p thing)
       (every (lambda (x) (typep x 'ty)) thing)))

(deftype type-list ()
  '(satisfies type-list-p))

(defstruct (node (:constructor nil))
  (derived-type nil :type (or null ty)))

(defun node-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'node-p x)))

(deftype node-list ()
  '(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  '(satisfies symbol-list-p))

(defmacro define-node-type (name &body slots)
  (multiple-value-bind (slots decls doc) (alexandria:parse-body slots :documentation t)
    (declare (ignore decls))
    `(defstruct (,name (:include node)
                       (:constructor ,name ,(mapcar #'first slots)))
       ,@(if (null doc) nil (list doc))
       ,@(loop :for (slot-name slot-type) :in slots
               :collect `(,slot-name nil :type ,slot-type :read-only t)))))


;;;;;;;;;;;;;;;;;;;;;;;;; The types of nodes ;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype literal-value ()
  "Allowed literal values as Lisp objects."
  '(or integer string))

(define-node-type node-literal
  "A literal value. These include things like integers and strings."
  (value literal-value))

(define-node-type node-variable
  (name symbol))

(define-node-type node-application
  (rator node)
  (rands node-list))

(define-node-type node-abstraction
  (vars symbol-list)
  (subexpr node))

(define-node-type node-let
  (bindings binding-list)
  (subexpr node))

;; TODO: Multiple bindings!
(define-node-type node-letrec
  (bindings binding-list)
  (subexpr node))

(define-node-type node-if
  (test node)
  (then node)
  (else node))

(define-node-type node-sequence
  (exprs node-list))

(define-node-type node-lisp
  (type ty)
  (form t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Match Nodes ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Patterns

(defstruct (ctor-pattern (:constructor ctor-pattern (ctor variables)))
  (ctor (required 'ctor) :type symbol :read-only t)
  variables)

(defun unparse-pattern (pat)
  (etypecase pat
    (ctor-pattern (let ((vars (ctor-pattern-variables pat)))
                    (if (endp vars)
                        (ctor-pattern-ctor pat)
                        `(,(ctor-pattern-ctor pat) ,@vars))))))

;;; Clauses

(defstruct match-clause
  (pattern nil :read-only t)
  (value (required 'value) :type node :read-only t))

(defun match-clause-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'match-clause-p x)))

(deftype match-clause-list ()
  '(satisfies match-clause-list-p))

(defun unparse-match-clause (mc)
  (check-type mc match-clause)
  `(,(unparse-pattern (match-clause-pattern mc))
    ,(unparse-node (match-clause-value mc))))

;;; The match node

(define-node-type node-match
  (value node)
  (tycon tycon)
  (clauses match-clause-list))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unparsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unparse-node (node)
  "Reconstruct the S-expression representation of a node NODE."
  (etypecase node
    (node-literal
     (node-literal-value node))
    (node-variable
     (node-variable-name node))
    (node-application
     `(,(unparse-node (node-application-rator node))
       ,@(mapcar #'unparse-node (node-application-rands node))))
    (node-abstraction
     `(coalton:fn ,(node-abstraction-vars node)
        ,(unparse-node (node-abstraction-subexpr node))))
    (node-let
     `(coalton:let ,(loop :for (var . val) :in (node-let-bindings node)
                          :collect (list var (unparse-node val)))
        ,(unparse-node (node-let-subexpr node))))
    (node-letrec
     `(coalton:letrec ,(loop :for (var . val) :in (node-letrec-bindings node)
                             :collect (list var (unparse-node val)))
        ,(unparse-node (node-letrec-subexpr node))))
    (node-if
     `(coalton:if ,(unparse-node (node-if-test node))
                  ,(unparse-node (node-if-then node))
                  ,(unparse-node (node-if-else node))))
    (node-sequence
     `(coalton:progn
        ,@(mapcar #'unparse-node (node-sequence-exprs node))))
    (node-lisp
     `(coalton:lisp ,(unparse-type (node-lisp-type node))
        ,(node-lisp-form node)))
    (node-match
     `(coalton:match ,(unparse-node (node-match-value node))
        ,@(mapcar #'unparse-match-clause (node-match-clauses node))))))
