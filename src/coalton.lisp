;;;; coalton.lisp

(in-package #:coalton-impl)

(defvar *print-coalton* nil
  "Print Coalton things as in Coalton.")

(defun error-parsing (thing reason-control &rest reason-args)
  (error "Failed to parse ~S because: ~?" thing reason-control reason-args))

;;; # Hindley-Milner Types
;;;
;;; ## Monotypes
;;;
;;; t := a           [Variable]
;;;    | C t ... t   [Application]

(deftype monotype ()
  '(or type-variable type-application))

(define-global-var **interned-type-variables** (tg:make-weak-hash-table :weakness ':value)
  "Table holding a mapping between symbols and TYPE-VARIABLEs.")

(defstruct (type-variable (:constructor %type-variable (symbol)))
  "A bare type variable. Required to be within a quantifier."
  (symbol (required 'symbol) :type symbol :read-only t))

(defun type-variable (symbol)
  (check-type symbol symbol)
  (alexandria:if-let ((tyvar (gethash symbol **interned-type-variables**)))
    tyvar
    (setf (gethash symbol **interned-type-variables**) (%type-variable symbol))))

;;; Beware, don't use (VECTOR MONOTYPE)! It will "upgrade" (more like
;;; downgrade, am I right?) to (VECTOR T).
(defun monotype-vector-p (v)
  (and (vectorp v)
       (every (lambda (x) (typep x 'monotype)) v)))

(deftype monotype-vector ()
  '(satisfies monotype-vector-p))

(defstruct (type-application (:constructor type-application (constructor arguments)))
  "A type application."
  (constructor (required 'constructor))
  (arguments   (required 'arguments)   :type monotype-vector :read-only t))


;;; ## Polytypes
;;;
;;; s := t
;;;    | forall a . s  [Quantifier]

(deftype polytype ()
  '(or monotype type-quantifier))

(defstruct (type-quantifier (:constructor type-quantifier (variable expression)))
  "Quantification of a type expression over a type variable."
  (variable   (required 'variable)   :type type-variable :read-only t)
  (expression (required 'expression) :type polytype      :read-only t))

(defun well-formed-type-p (ty)
  "Is the type TY a well-formed type?"
  (labels ((recurse (ty vars)
             (etypecase ty
               ;; We do *NOT* allow implicit universal quantification
               ;; of free variables.
               (type-quantifier
                (recurse (type-quantifier-expression ty)
                         (union vars (list (type-quantifier-variable ty)))))
               (type-variable
                (boolify (member (type-variable-symbol ty) vars)))
               ;; TODO: check arity when we got the data.
               (type-application
                (every (lambda (arg) (recurse arg vars))
                       (type-application-arguments ty))))))
    (recurse ty nil)))

;;; # Info Database

(defstruct entry
  declared-type
  derived-type
  source-form)

(define-global-var **global-value-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton global value definitions.")

(defun var-knownp (var)
  (check-type var symbol)
  (nth-value 1 (gethash var **global-value-definitions**)))

(defun var-info (var)
  (check-type var symbol)
  (multiple-value-bind (val exists?) (gethash var **global-value-definitions**)
    (unless exists?
      (error "Could not retrieve the type of ~S because it is unknown." var))
    val))

(defun (setf var-info) (new-value var)
  (check-type new-value entry)
  (check-type var symbol)
  (when (var-knownp var)
    (warn "Overwriting info entry for ~S" var))
  (setf (gethash var **global-value-definitions**) new-value))

(defun forward-declare-variable (var)
  (check-type var symbol)
  (when (var-knownp var)
    (error "Can't forward declare ~S, which is already known." var))
  (setf (gethash var **global-value-definitions**) (make-entry)))

(defun var-declared-type (var)
  (let ((info (var-info var)))
    (entry-declared-type info)))

(defun (setf var-declared-type) (new-value var)
  (let ((info (var-info var)))
    (when (entry-declared-type info)
      (warn "Overwriting declared type of ~S" var))
    (setf (entry-declared-type info) new-value)))

(defun var-derived-type (var)
  (let ((info (var-info var)))
    (entry-derived-type info)))

(defun (setf var-derived-type) (new-value var)
  (let ((info (var-info var)))
    (when (entry-derived-type info)
      (warn "Overwriting derived type of ~S" var))
    (setf (entry-derived-type info) new-value)))


;;; A type constructor is not a constructor for a value, but a
;;; constructor for a *type*! Get with the program!
(defstruct type-constructor
  (name (required 'name) :type symbol        :read-only t)
  (arity 0               :type unsigned-byte :read-only t))

;;; TODO: Aliases.

(define-global-var **type-definitions**
    (make-hash-table :test 'eql)
  "Database of Coalton type definitions. These are mappings from symbols to type constructors.")

;;; Some initial type constructors.
;;;
;;; Hindley-Milner definitely needs the -> constructor!
(setf (gethash 'coalton:-> **type-definitions**)
      (make-type-constructor :name 'coalton:->
                             :arity 2))

(defun type-knownp (tyname)
  (check-type tyname symbol)
  (nth-value 1 (gethash tyname **type-definitions**)))

(defun find-type (tyname)
  (values (gethash tyname **type-definitions**)))

(defun parse-type-expression (whole-expr)
  (labels ((parse (expr bound seen allow-quantifiers)
             (etypecase expr
               (symbol
                (cond
                  ((member expr bound)
                   (values (type-variable expr)
                           bound
                           (cons expr seen)))

                  ((type-knownp expr)
                   ;; Should be a 0-arity constructor.
                   (let ((tycon (find-type expr)))
                     ;; TODO: This could possibly be an alias, in
                     ;; which case it won't have arity.
                     (unless (zerop (type-constructor-arity tycon))
                       (error-parsing whole-expr
                                      "The type ~S shows up as if it is ~
                                       a nullary type constructor, but ~
                                       in fact, it should have an arity ~
                                       of ~D."
                                      expr (type-constructor-arity tycon))))
                   (values
                    (type-application expr #())
                    bound
                    seen))

                  (t
                   (error-parsing whole-expr
                                  "Couldn't resolve the type ~S"
                                  expr))))
               (list
                (alexandria:destructuring-case expr
                  ((coalton:forall var subexpr)
                   ;; Don't allow nested quantification.
                   (unless allow-quantifiers
                     (error-parsing whole-expr "Illegal nested quantifier."))
                   ;; Eliminate redundant quantifiers.
                   (if (member var bound)
                       (values (parse subexpr bound seen allow-quantifiers))
                       (multiple-value-bind (result new-bound new-seen)
                           (parse subexpr (cons var bound) seen t)
                         ;; If we didn't see our variable, then we don't
                         ;; need the quantifier.
                         (if (not (member var new-seen))
                             (values result new-bound new-seen)
                             (values (type-quantifier (type-variable var) result)
                                     new-bound
                                     new-seen)))))
                  ((t &rest tyargs)
                   (let ((tycon-name (first expr)))
                     (unless (type-knownp tycon-name)
                       (error-parsing whole-expr
                                      "Unknown type constructor ~S"
                                      tycon-name))
                     (let* ((tycon (find-type tycon-name))
                            (arity (type-constructor-arity tycon)))
                       (unless (= arity (length tyargs))
                         (error-parsing whole-expr
                                        "The type expression ~S has the wrong ~
                                         arity. Got ~D argument~:P when I ~
                                         expected ~D."
                                        expr
                                        (length tyargs)
                                        arity))

                       ;; Go through and parse everything out.
                       (let ((all-bound bound)
                             (all-seen seen)
                             (arguments (make-array arity :initial-element nil)))
                         (loop :for i :from 0
                               :for tyarg :in tyargs
                               :do (multiple-value-bind (result new-bound new-seen)
                                       (parse tyarg all-bound all-seen nil)
                                     (setf all-bound new-bound
                                           all-seen new-seen
                                           (aref arguments i) result)))
                         ;; Return the constructed type.
                         (values (type-application tycon-name arguments)
                                 all-bound
                                 all-seen))))))))))
    (multiple-value-bind (type bound seen) (parse whole-expr nil nil t)
      ;; Check for unquantified variables, and optionally quantify
      ;; them.
      (alexandria:when-let ((free (set-difference seen bound)))
        (cerror "Quantify the type."
                "Parsed a type with unquantified free variables: ~{~S~^, ~}."
                free)
        (dolist (free-var free)
          (setf type (type-quantifier (type-variable free-var) type))))
      ;; Return the parsed type.
      type)))

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
;;; This is a template:
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
