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
;;;
;;; TODO: Some syntax isn't accounted for:
;;;
;;;          - LETREC
;;;          - Top-level syntax
;;;          - All of the desired atomic data
;;;          - Variable declarations
;;;          - Literal syntax for some constructors
;;;

(defun compile-value-to-lisp (form)
  "Compile the Coalton form FORM into Lisp code."
  ;; XXX: Doesn't make use of type info yet!
  (labels ((analyze (expr env)
             (cond
               ((atom expr)
                (etypecase expr
                  (null    (error-parsing expr "NIL is not allowed!"))
                  (symbol  (analyze-variable expr env))
                  (integer (analyze-atom expr env))))
               ((alexandria:proper-list-p expr)
                (alexandria:destructuring-case expr
                  ((coalton:fn var subexpr)
                   (analyze-abstraction var subexpr env))
                  ((coalton:let bindings subexpr)
                   (analyze-let bindings subexpr env))
                  ((coalton:if test then else)
                   (analyze-if test then else env))
                  ((coalton:lisp type lisp-expr)
                   (analyze-lisp type lisp-expr env))
                  ((coalton:match expr &rest patterns)
                   (analyze-match expr patterns env))
                  ((coalton:progn &rest exprs)
                   (analyze-sequence exprs env))
                  ((t &rest rands)
                   (analyze-application (first expr) rands env))))
               (t (error-parsing expr "The expression is not a valid value expression."))))

           (analyze-atom (atom env)
             (declare (ignore env))
             ;; Just return the atom.
             atom)
           (analyze-variable (var env)
             (declare (ignore env))
             ;; Just return the variable.
             ;;
             ;; XXX: Do we have to special-case any variable? We can
             ;; statically detect if it's not used. But so can the
             ;; compiler.
             var)
           (analyze-abstraction (var subexpr env)
             ;; XXX: a VAR with an &-name will cause breakage
             `(cl:lambda (,var) ,(analyze subexpr env)))
           (analyze-let (bindings subexpr env)
             `(cl:let ,(loop :for (bind-var bind-val) :in bindings
                             :collect `(,bind-var ,(analyze bind-val env)))
                ,(analyze subexpr env)))
           (analyze-if (test then else env)
             `(cl:if ,(analyze test env)
                     ,(analyze then env)
                     ,(analyze else env)))
           (analyze-lisp (type lisp-expr env)
             (declare (ignore type env))
             lisp-expr)
           (analyze-match (expr patterns env)
             (declare (ignore expr patterns env))
             (error "TODO: unsupported"))
           (analyze-sequence (exprs env)
             `(progn
                ,@(loop :for expr :in exprs
                        :collect (analyze expr env))))
           (analyze-application (rator rands env)
             ;;; XXX: We should special-case for global function
             ;;; symbols and call them in the Lisp-2 fashion.
             `(cl:funcall ,(analyze rator env)
                          ,@(loop :for rand :in rands :collect (analyze rand env)))))
    (analyze form nil)))



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

(defun parse-define-type-alias-form (form)
  "Parse a COALTON:DEFINE-TYPE-ALIAS form."
  (check-compound-form form 'coalton:define-type-alias)
  (check-compound-form-length form 3)
  (destructuring-bind (def-symbol tyname type-expr) form
    (declare (ignore def-symbol))
    (unless (symbolp tyname)
      (error-parsing form "The second argument should be a symbol."))
    (values tyname (parse-type-expression type-expr))))

(defmethod compile-toplevel-special-form ((operator (eql 'coalton:define-type-alias)) whole)
  (multiple-value-bind (tyname type) (parse-define-type-alias-form whole)
    ;; Establish the alias as a side-effect.
    ;;
    ;; TODO: Make this better. Actually check things, and abstract out
    ;; the whole table access dealio here.
    (setf (gethash tyname **type-definitions**) `(alias ,type))
    ;; Produce no code.
    (values)))

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
  (declare (ignore val))
  (check-type var symbol)
  (error "..."))

(defun parse-define-form-function (fvar args val)
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (declare (ignore fvar args val))
  (error "..."))

(defmethod compile-toplevel-special-form ((operator (eql 'coalton:define)) whole)
  (declare (ignore whole))
  )


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
