;;;; parse-type.lisp

(in-package #:coalton-impl)

;;; Grammar:
;;;
;;;    <type> :=  <type expr>
;;;            | (for <constraint>* => <type expr>)
;;;
;;;    <constraint> := (<class name> <type variable>)
;;;
;;;    <type expr> := <type alias>                       ; TODO!
;;;                 | <type variable>
;;;                 | <nullary type constructor>
;;;                 | (fn <type expr>* -> <type-expr>)
;;;                 | (<type constructor> <type expr>*)

(defstruct type-parsing-context
  "Context for parsing coalton types"
  (variable-assignments (make-hash-table))
  (extra-tycons nil)
  (allow-new-variables t))

(defun parse-type-expression (whole-expr &key variable-assignments
                                           extra-tycons
                                           (allow-new-variables t))
  "Parse the type expression WHOLE-EXPR. Return two values:

1. The parsed expression.

2. An a-list of symbol -> TYVAR pairs.

VARIABLE-ASSIGNMENTS is an alist of (SYMBOL TYVAR) pairs.

EXTRA-TYCONS is a list of tycons that are perhaps not globally defined yet. These will be preferred over global definitions.
"
  ;; TODO: We need to allow this to change and make sure all fields are valid
  (let ((context (make-type-parsing-context :variable-assignments (if variable-assignments
                                                                      (alexandria:alist-hash-table variable-assignments)
                                                                      (make-hash-table))
                                            :extra-tycons extra-tycons
                                            :allow-new-variables allow-new-variables)))
    ;; A constrained type can only be at the "top level".
    (with-parsing-context (whole-expr)
      (values (if (and (alexandria:proper-list-p whole-expr)
                       (not (endp whole-expr))
                       (eq 'coalton:for (first whole-expr)))
                  (parse-constrained-type whole-expr context)
                  (parse-type whole-expr context))
              (alexandria:hash-table-alist (type-parsing-context-variable-assignments context))))))

(defun parse-type (expr context)
  (check-type context type-parsing-context)
  (typecase expr
    ;; TODO: Allow () for something useful? Maybe unit?
    (null
     (error-parsing expr "Invalid type expression: ~S" expr))

    (symbol
     (if (type-knownp expr context)
         (parse-nullary-constructor expr context)
         (parse-type-variable expr context)))

    (alexandria:proper-list
     (parse-type-application expr context))

    (t
     (error-parsing expr "Invalid type expression: ~S" expr))))

(defun parse-nullary-constructor (expr context)
  (check-type context type-parsing-context)
  (check-type expr symbol)
  (unless (type-knownp expr context)
    (error-parsing expr "Unknown type constructor ~S" expr))
  (let ((parsed-tycon (type-find-it expr context)))
    (unless (= 0 (tycon-arity parsed-tycon))
      (error-parsing expr "Invalid number of arguments for constructor ~S. Wanted ~D but got 0." expr (tycon-arity parsed-tycon)))
    (tyapp parsed-tycon)))

(defun parse-type-application (expr context)
  (check-type context type-parsing-context)
  (cond
    ;; Old syntax for doing function types not supported.
    ((eq 'coalton:-> (first expr))
     (error-parsing expr "Function types have syntax (FN <ty>* -> <ty>). Got: ~S" expr))

    ;; New syntax for doing function types.
    ((eq 'coalton:fn (first expr)) (parse-function-type expr context))

    ;; Constrained types aren't valid here.
    ((eq 'coalton:for (first expr))
     (error-parsing expr "Constrained types can't be embedded in a larger type."))

    ;; Other applications.
    (t
     (destructuring-bind (tycon &rest args) expr
       (unless (symbolp tycon)
         (error-parsing expr "Invalid part of type expression: ~S" tycon))
       (unless (type-knownp tycon context)
         (error-parsing expr "Unknown type constructor ~S" tycon))
       ;; TODO: Make sure arity is correct!
       (let ((parsed-tycon (type-find-it tycon context)))
         (unless (= (tycon-arity parsed-tycon)
                    (length args))
           (error-parsing expr "Invalid number of arguments for constructor ~S. Wanted ~D but got ~D." tycon (tycon-arity parsed-tycon) (length args)))
         (apply #'tyapp
                parsed-tycon
                (mapcar (lambda (x) (parse-type x context)) args)))))))

(defun parse-type-variable (expr context)
  "Parse a type variable in EXPR"
  (check-type context type-parsing-context)
  (check-type expr symbol)
  (let ((table (type-parsing-context-variable-assignments context)))
    (or (gethash expr table)
        (if (type-parsing-context-allow-new-variables context)
            (setf (gethash expr table) (make-variable expr))
            (error-parsing expr "Unknown type variable ~S" expr)))))

(defun parse-function-type (expr context)
  (check-type context type-parsing-context)
  (let ((arrow (position 'coalton:-> expr)))
    (when (null arrow)
      (error-parsing expr "Invalid function type because it lacks an arrow: ~S" expr))
    (let ((from (subseq expr 1 arrow))  ; exclude FN symbol
          (to   (subseq expr (1+ arrow))))
      (cond
        ((null to) (error-parsing expr "Can't have an empty return type in function type: ~S" expr))
        ((not (null (rest to))) (error-parsing expr "Can't have more than one return type in function type: ~S" expr)))
      ;; parse out the input and output types
      (setf from (mapcar (lambda (x) (parse-type x context)) from))
      (setf to   (parse-type (first to) context))
      ;; return the parsed type
      (tyfun from to))))

(defun parse-constrained-type (expr context)
  (check-type context type-parsing-context)
  (multiple-value-bind (from to)
      (parse-arrow 'coalton:=> (rest expr) ; exclude FOR symbol
                   :error (lambda ()
                            (error-parsing expr "Invalid constrained type ~
                                                 because it lacks an ~
                                                 arrow '=>': ~S" expr)))
    (cond
      ((null to) (error-parsing expr "Constrained type requires a type to constrain: ~S" expr))
      ((not (null (rest to))) (error-parsing expr "Only one type can be constrained: ~S" expr)))

    (if (endp from)
        (parse-type (first to) context)
        (cty (parse-type (first to) context)
             :constraints (mapcar (lambda (x) (parse-type-constraint x context)) from)))))

(defun parse-type-constraint (constraint context)
  (check-type context type-parsing-context)
  (unless (and (alexandria:proper-list-p constraint)
               (= 2 (length constraint)))
    (error-parsing constraint "Invalid constraint: ~S" constraint))
  (destructuring-bind (class-name-expr variable-expr) constraint
    ;; Check that the class name is a symbol
    (unless (symbolp class-name-expr)
      (error-parsing constraint "Invalud constraint: ~S" constraint))
    (let ((class-name class-name-expr)
          (var (parse-type variable-expr context))) ;;;;;;;;;;;;;;;; TODO ;;;;;;;;;;;;;;
      (unless (typep var 'tyvar)
        (error-parsing constraint "Invalid constraint: ~S" constraint))
      (cx class-name var))))

;; TODO: This is only used twice. Is an abstraction _really_ needed?
(defun parse-arrow (arrow list &key error)
  "Parse a list that looks like (... ARROW ...) into two sub-lists, one of elements before the arrow, and one of elements after the arrow. ARROW can really be any symbol. If the arrow isn't found, call ERROR."
  (let ((arrow-position (position arrow list)))
    (when (null arrow-position)
      (funcall error))
    (values (subseq list 0 arrow-position)
            (subseq list (1+ arrow-position)))))

(defun type-knownp (name context)
  (check-type context type-parsing-context)
  (or (find name (type-parsing-context-extra-tycons context) :key #'tycon-name)
      (tycon-knownp name)))

(defun type-find-it (name context)
  (check-type context type-parsing-context)
  (or (find name (type-parsing-context-extra-tycons context) :key #'tycon-name)
      (find-tycon name)))
