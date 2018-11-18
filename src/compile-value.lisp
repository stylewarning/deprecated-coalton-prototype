;;;; compile-value.lisp

(in-package #:coalton-impl)

;;; ## Value Analysis
;;;
;;; For values, we follow the usual grammar for the simply typed
;;; lambda calculus, with a modicum of practical extensions. The
;;; precise grammar is:
;;;
;;;     <atom> := <CL INTEGER>
;;;             | <CL STRING>
;;;             | ...
;;;
;;;     <expr> := <atom>
;;;             | <variable>         ; variable
;;;             | (<expr> <expr> ...)
;;;                                  ; application
;;;             | (fn (<variable> ...) <expression>)
;;;                                  ; abstraction
;;;             | (let ((<variable> <expression>) ...) <expression>)
;;;                                  ; lexical binding
;;;             | (if <expr> <expr> <expr>)
;;;                                  ; conditional
;;;             | (progn <expr> ...) ; sequence
;;;             | (lisp <type> <expr>)
;;;                                  ; Lisp escape
;;;             | (letrec ((<variable> <expression>) ...) <expression>)
;;;
;;; TODO: Some syntax isn't accounted for:
;;;
;;;          - Top-level syntax
;;;          - All of the desired atomic data
;;;          - Variable declarations
;;;          - Literal syntax for some constructors
;;;

(defun compile-value-to-lisp (value)
  "Compile the node VALUE into Lisp."
  (check-type value node)
  (labels ((analyze (expr)
             (etypecase expr
               (node-literal
                (node-literal-value expr))

               (node-variable
                (node-variable-name expr))

               (node-abstraction
                (let ((vars (node-abstraction-vars expr)))
                  `(lambda  (,@vars)
                     (declare (ignorable ,@vars))
                     ,(analyze (node-abstraction-subexpr expr)))))

               (node-let
                `(let ,(loop :for (var . val) :in (node-let-bindings expr)
                             :collect `(,var ,(analyze val)))
                   ,(analyze (node-let-subexpr expr))))

               (node-letrec
                ;; TODO: this is broken... this isn't quite
                ;; right and only works well for functions
                (let* ((bindings (node-letrec-bindings expr))
                       (subexpr (node-letrec-subexpr expr))
                       (vars (mapcar #'car bindings))
                       (vals (mapcar #'cdr bindings)))
                  `(let (,@vars)
                     (psetf ,@(loop :for var :in vars
                                    :for val :in vals
                                    :collect var
                                    :collect (analyze val)))
                     ,(analyze subexpr))))

               (node-if
                `(if (eql coalton:true ,(analyze (node-if-test expr)))
                     ,(analyze (node-if-then expr))
                     ,(analyze (node-if-else expr))))

               (node-lisp
                (node-lisp-form expr))

               (node-sequence
                `(progn
                   ,@(mapcar #'analyze (node-sequence-exprs expr))))

               (node-application
                (let ((rator (analyze (node-application-rator expr)))
                      (rands (mapcar #'analyze (node-application-rands expr))))
                  `(funcall ,rator ,@rands))))))
    (analyze value)))
