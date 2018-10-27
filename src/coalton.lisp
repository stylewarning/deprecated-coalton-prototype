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

(defstruct (type-variable (:constructor type-variable (symbol)))
  "A bare type variable. Required to be within a quantifier."
  (symbol (required 'symbol) :type symbol :read-only t))

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

(define-global-var **type-definitions**
  (make-hash-table :test 'eql)
  "Database of Coalton type definitions.")

(defun parse-type-expression (expr)
  ;; TODO: Actually parse it out, yo.
  expr)

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
