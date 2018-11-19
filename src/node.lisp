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
  `(satisfies node-list-p))

(defun binding-list-p (x)
  (and (alexandria:proper-list-p x)
       (every (lambda (b) (typep b '(cons symbol node))) x)))

(deftype binding-list ()
  `(satisfies binding-list-p))

(defun symbol-list-p (x)
  (and (alexandria:proper-list-p x)
       (every #'symbolp x)))

(deftype symbol-list ()
  `(satisfies symbol-list-p))

(defmacro define-node-type (name &body slots)
  (multiple-value-bind (slots decls doc) (alexandria:parse-body slots :documentation t)
    (declare (ignore decls))
    `(defstruct (,name (:include node)
                       (:constructor ,name ,(mapcar #'first slots)))
       ,@(if (null doc) nil (list doc))
       ,@(loop :for (slot-name slot-type) :in slots
               :collect `(,slot-name nil :type ,slot-type :read-only t)))))


;;;;;;;;;;;;;;;;;;;;;;;;; The types of nodes ;;;;;;;;;;;;;;;;;;;;;;;;;

(define-node-type node-literal
  (value t))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unparsing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unparse-node (node)
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
        ,(node-lisp-form node)))))
