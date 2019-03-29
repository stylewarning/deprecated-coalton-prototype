;;;; parse-value.lisp

(in-package #:coalton-impl)

;;; For values, we follow the usual grammar for the simply typed
;;; lambda calculus, with a modicum of practical extensions. The
;;; precise grammar is:
;;;
;;;     <atom> := <CL INTEGER>
;;;             | <CL STRING>
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

(defun parse-form (form)
  "Parse the value form FORM into a NODE structure. This also performs macro-expansion.

This does not attempt to do any sort of analysis whatsoever. It is suitable for parsing expressions irrespective of environment."
  (labels ((parse (expr)
             (cond
               ((atom expr)
                (etypecase expr
                  (null    (error-parsing expr "NIL is not allowed!"))
                  (symbol  (parse-variable expr))
                  (literal-value
                   (parse-atom expr))))
               ((alexandria:proper-list-p expr)
                (alexandria:destructuring-case expr
                  ((coalton:fn vars subexpr)
                   (parse-abstraction vars subexpr))
                  ((coalton:let bindings subexpr)
                   (parse-let bindings subexpr))
                  ((coalton:letrec bindings subexpr)
                   (parse-letrec bindings subexpr))
                  ((coalton:if test then else)
                   (parse-if test then else))
                  ((coalton:lisp type lisp-expr)
                   (parse-lisp type lisp-expr))
                  ((coalton:progn &rest exprs)
                   (parse-sequence exprs))
                  ((t &rest rands)
                   (parse-application (first expr) rands))))
               (t (error-parsing expr "The expression is not a valid value expression."))))

           (parse-atom (atom)
             (node-literal atom))

           (parse-variable (var)
             (node-variable var))

           (parse-abstraction (vars subexpr)
             (node-abstraction (alexandria:ensure-list vars)
                               (parse subexpr)))

           (parse-let (bindings subexpr)
             (node-let (loop :for (bind-var bind-val) :in bindings
                             :collect (cons bind-var (parse bind-val)))
                       (parse subexpr)))

           (parse-letrec (bindings subexpr)
             (node-letrec (loop :for (bind-var bind-val) :in bindings
                                :collect (cons bind-var (parse bind-val)))
                          (parse subexpr)))

           (parse-if (test then else)
             (node-if (parse test)
                      (parse then)
                      (parse else)))

           (parse-lisp (type lisp-expr)
             ;; Do *NOT* parse LISP-EXPR!
             (node-lisp (parse-type-expression type) lisp-expr))

           (parse-sequence (exprs)
             (node-sequence (loop :for expr :in exprs :collect (parse expr))))

           (parse-application (rator rands)
             (cond
               ((and (symbolp rator) (macro-function rator))
                (let ((expansion (funcall (macro-function rator) (cons rator rands) nil)))
                  (parse expansion)))
               (t
                (node-application
                 (parse rator)
                 (loop :for rand :in rands :collect (parse rand)))))))
    (parse form)))
