;;;; coalton.lisp

(in-package #:coalton-impl)

;;; # Compiler
;;;
;;; The compiler is a combination of a code analyzer and code
;;; generator. The main analysis to be done is type checking. The code
;;; generator just generates valid Common Lisp, to be further
;;; processed by the Common Lisp compiler. Generally, this code will
;;; be generated at macroexpansion time of the ambient Common Lisp
;;; compiler. See the COALTON macro.

(define-global-var **toplevel-operators** '(coalton:progn
                                            coalton:coalton))
(define-global-var **special-operators** `(,@**toplevel-operators**
                                           coalton:define
                                           coalton:define-type-alias
                                           coalton:define-type
                                           coalton:declare))

;;; ## Value Analysis
;;;
;;; For values, we follow the usual grammar for the simply typed
;;; lambda calculus, with a modicum of practical extensions. The
;;; precise grammar is:
;;;
;;;     <atom> := <CL Integer>
;;;             | ...
;;;
;;;     <expr> := <atom>
;;;             | <variable>         ; variable
;;;             | (<expr> <expr>)    ; application
;;;             | (fn <variable> <expression>)
;;;                                  ; abstraction
;;;             | (let ((<variable> <expression>) ...) <expression>)
;;;                                  ; lexical binding
;;;             | (if <expr> <expr> <expr>
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
                `(lambda  (,(node-abstraction-var expr))
                   ,(analyze (node-abstraction-subexpr expr))))

               (node-let
                `(let ,(loop :for (var . val) :in (node-let-bindings expr)
                             :collect `(,var ,(analyze val)))
                   ,(analyze (node-let-subexpr expr))))

               (node-letrec
                ;; TODO: fixme? this is broken... this isn't quite
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
                `(if ,(analyze (node-if-test expr))
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

;;; ## Compilation

(defun compile-toplevel-form (form)
  (cond
    ;; An atomic form at the top-level. Consider me spooked.
    ;;
    ;; TODO: Actually do something proper here.
    ((atom form)
     (error "Atomic form ~S found at the top-level." form))

    ((member (first form) **special-operators**)
     (compile-toplevel-special-form (first form) form))

    (t
     (error "I don't know how to deal with non-special forms."))))

(defgeneric compile-toplevel-special-form (operator whole))

(defmethod compile-toplevel-special-form ((operator (eql 'coalton:progn)) whole)
  (error "PROGN should be elminiated at the top-level."))

(defmethod compile-toplevel-special-form ((operator (eql 'coalton:coalton)) whole)
  (error "COALTON should be eliminated at the top-level."))

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

(defmethod compile-toplevel-special-form ((operator (eql 'coalton:declare)) whole)
  (multiple-value-bind (var type) (parse-declare-form whole)
    ;; This just has compile-time effects. It doesn't produce
    ;; executable code.
    (unless (var-knownp var)
      (forward-declare-variable var))
    (setf (var-declared-type var) type)
    ;; Produce no code.
    (values)))

#+#:ignore
(defun parse-define-type-alias-form (form)
  "Parse a COALTON:DEFINE-TYPE-ALIAS form."
  (check-compound-form form 'coalton:define-type-alias)
  (check-compound-form-length form 3)
  (destructuring-bind (def-symbol tyname type-expr) form
    (declare (ignore def-symbol))
    (unless (symbolp tyname)
      (error-parsing form "The second argument should be a symbol."))
    (values tyname (parse-type-expression type-expr))))

#+#:ignore
(defmethod compile-toplevel-special-form ((operator (eql 'coalton:define-type-alias)) whole)
  (multiple-value-bind (tyname type) (parse-define-type-alias-form whole)
    ;; Establish the alias as a side-effect.
    ;;
    ;; TODO: Make this better. Actually check things, and abstract out
    ;; the whole table access dealio here.
    (setf (gethash tyname **type-definitions**) `(alias ,type))
    ;; Produce no code.
    (values)))

(defun parse-define-type-form (form)
  (destructuring-bind (def-type type &rest ctors) form
    (assert (eql 'coalton:define-type def-type))
    (assert (not (null type)))
    (setf type (alexandria:ensure-list type))
    (destructuring-bind (tycon-name &rest tyvar-names) type
      (when (tycon-knownp tycon-name)
        (error "Already defined tycon: ~S" tycon-name))
      (assert (every #'symbolp tyvar-names))
      (let* ((arity (length tyvar-names))
             (tycon (make-tycon :name tycon-name :arity arity))
             (constructors nil))
        (multiple-value-bind (ty fvs) (parse-type-expression type :extra-tycons (list tycon))
          (dolist (ctor ctors)
            (typecase ctor
              (symbol
               (push (list ':variable ctor ty) constructors))
              (alexandria:proper-list
               (destructuring-bind (name argty) ctor
                 (push (list ':function
                             name
                             (make-function-type
                              (parse-type-expression argty :variable-assignments fvs)
                              ty))
                       constructors)))))
          (values tycon ty constructors fvs))))))

;;; XXX FIXME: consider using other types instead of structs.
(defmethod compile-toplevel-special-form ((operator (eql 'coalton:define-type)) whole)
  (multiple-value-bind (tycon ty ctors fvs) (parse-define-type-form whole)
    (declare (ignore ty fvs))
    (let ((super-name (tycon-name tycon)))
      `(progn
         (defstruct (,super-name (:constructor nil)))
         ,@(loop :for (kind sub-name ty) :in (mapcar #'first ctors)
                 :append (ecase kind
                           (:variable
                            (list
                             `(defstruct (,sub-name (:include ,super-name)))
                             ;; XXX FIXME
                             `(define-global-var ,sub-name nil)))
                           (:function
                            (list
                             `(defstruct (,sub-name (:include ,super-name))
                                val)))))))))

(defun parse-define-form (form)
  "Parse a COALTON:DEFINE form."
  (check-compound-form form 'coalton:define)
  (check-compound-form-length form 3)
  ;; Defines either define a value or a function. Values and functions
  ;; in Coalton occupy the namespace, but the intent of the user can
  ;; be distinguished. A definition either looks like:
  ;;
  ;;     (DEFINE <var> <val>)
  ;;
  ;; or
  ;;
  ;;     (DEFINE (<fvar> <arg>*) <val>)
  ;;
  ;; The former defines a variable, the latter defines a function.
  (destructuring-bind (def-symbol var-thing val) form
    (declare (ignore def-symbol))
    (cond
      ((null var-thing)
       (error-parsing form "Found a null value where a symbol or function ~
                            was expected."))
      ((symbolp var-thing)
       (parse-define-form-variable var-thing val))
      ((and (listp var-thing)
            (every #'symbolp var-thing))
       (parse-define-form-function (first var-thing) (rest var-thing) val))
      (t
       (error-parsing form "Invalid second argument.")))))

(defun parse-define-form-variable (var val)
  ;; The (DEFINE <var> <val>) case.
  (check-type var symbol)
  ;; XXX: Should this be LETREC too? Probably for something like F = x => ... F.
  (values var (parse-form val)))

(defun parse-define-form-function (fvar args val)
  (check-type fvar symbol)
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (values fvar (parse-form
                `(coalton:letrec ,fvar
                                 ,(if (null args)
                                      val
                                      (loop :with thing := val
                                            :for var :in (reverse args)
                                            :do (setf thing `(coalton:fn ,var ,thing))
                                            :finally (return thing)))
                                 ,fvar))))

;;; TODO: make sure we can lexically shadow global bindings
(defmethod compile-toplevel-special-form ((operator (eql 'coalton:define)) whole)
  (multiple-value-bind (name expr) (parse-define-form whole)
    (cond
      ((var-definedp name)
       ;; XXX: Get this right. Re-typecheck everything?!
       (let ((internal-name (entry-internal-name (var-info name))))
         `(setf ,internal-name ,(compile-value-to-lisp expr))))
      (t
       (unless (var-knownp name)
         ;; Declare the variable.
         (forward-declare-variable name))
       ;; Do some type inferencing.
       (let ((inferred-type (derive-type expr))
             (internal-name (entry-internal-name (var-info name))))
         ;; FIXME check VAR-DECLARED-TYPE
         ;; FIXME check VAR-DERIVED-TYPE
         (setf (var-derived-type name)             inferred-type
               (entry-source-form (var-info name)) whole
               (entry-node (var-info name))        expr)
         `(progn
            (define-symbol-macro ,name ,internal-name)
            (global-vars:define-global-var ,internal-name ,(compile-value-to-lisp expr))))))))


;;; Entry Point

(defun flatten-toplevel-forms (forms)
  (loop :for form :in forms
        :append (cond
                  ((atom form) (list form))
                  ((member (first form) **toplevel-operators**)
                   (flatten-toplevel-forms (rest form)))
                  (t (list form)))))

(defun process-coalton-toplevel-forms (forms)
  `(progn ,@(mapcar #'compile-toplevel-form forms)))


;;; Coalton Macros

(defmacro coalton:coalton (&body toplevel-forms)
  (process-coalton-toplevel-forms (flatten-toplevel-forms toplevel-forms)))
