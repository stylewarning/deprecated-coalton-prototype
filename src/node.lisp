;;;; node.lisp

;;;; A representation of a value.

(in-package #:coalton-impl)

;;; Types are fully defined in types.lisp
(defstruct (ty (:constructor nil)))

(defstruct (node (:constructor nil)))

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

(defmacro define-node-type (name &body slots)
  (multiple-value-bind (slots decls doc) (alexandria:parse-body slots :documentation t)
    (declare (ignore decls))
    `(defstruct (,name (:include node)
                       (:constructor ,name ,(mapcar #'first slots)))
       ,@(if (null doc) nil (list doc))
       ,@(loop :for (slot-name slot-type) :in slots
               :collect `(,slot-name nil :type ,slot-type :read-only t)))))

(define-node-type node-literal
  (value t))

(define-node-type node-variable
  (name symbol))

(define-node-type node-application
  (rator node)
  (rands node-list))

(define-node-type node-abstraction
  (var symbol)
  (subexpr node))

(define-node-type node-let
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
