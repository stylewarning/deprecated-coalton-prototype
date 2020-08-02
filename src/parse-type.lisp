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

(defun parse-arrow (arrow list &key error)
  (let ((arrow-position (position arrow list)))
    (when (null arrow-position)
      (funcall error))
    (values (subseq list 0 arrow-position)
            (subseq list (1+ arrow-position)))))

(defun parse-type-expression (whole-expr &key variable-assignments
                                              extra-tycons)
  "Parse the type expression WHOLE-EXPR. Return two values:

1. The parsed expression.

2. An a-list of symbol -> TYVAR pairs.

VARIABLE-ASSIGNMENTS is an alist of (SYMBOL TYVAR) pairs.

EXTRA-TYCONS is a list of tycons that are perhaps not globally defined yet. These will be preferred over global definitions.
"
  ;; Below, TABLE is a mapping from symbols to fresh type variables.
  (let ((table (alexandria:alist-hash-table variable-assignments)))
    (labels ((knownp (name)
               (or (find name extra-tycons :key #'tycon-name)
                   (tycon-knownp name)))
             (find-it (name)
               (or (find name extra-tycons :key #'tycon-name)
                   (find-tycon name)))
             (parse-variable (expr)
               (check-type expr symbol)
               (or (gethash expr table)
                   (setf (gethash expr table) (make-variable))))

             (parse-nullary-constructor (expr)
               (check-type expr symbol)
               (unless (knownp expr)
                 (error-parsing whole-expr "Unknown type constructor ~S" expr))
               (tyapp (find-it expr)))

             (parse-function (expr)
               (multiple-value-bind (from to)
                   (parse-arrow 'coalton:-> (rest expr) ; exclude FN symbol
                                :error (lambda ()
                                         (error-parsing whole-expr "Invalid function type ~
                                                                    because it lacks an ~
                                                                    arrow: ~S" expr)))
                 (cond
                   ((null to) (error-parsing whole-expr "Can't have an empty return type in function type: ~S" expr))
                   ((not (null (rest to))) (error-parsing whole-expr "Can't have more than one return type in function type: ~S" expr)))
                 ;; parse out the input and output types
                 (setf from (mapcar #'parse from))
                 (setf to   (parse (first to)))
                 ;; return the parsed type
                 (tyfun from to)))

             (parse-application (expr)
               (cond
                 ;; Old syntax for doing function types not supported.
                 ((eq 'coalton:-> (first expr))
                  (error-parsing whole-expr "Function types have syntax (FN <ty>* -> <ty>). Got: ~S" expr))

                 ;; New syntax for doing function types.
                 ((eq 'coalton:fn (first expr)) (parse-function expr))

                 ;; Constrained types aren't valid here.
                 ((eq 'coalton:for (first expr))
                  (error-parsing whole-expr "Constrained types can't be embedded in a larger type."))

                 ;; Other applications.
                 (t
                  (destructuring-bind (tycon &rest args) expr
                    (unless (symbolp tycon)
                      (error-parsing whole-expr "Invalid part of type expression: ~S" tycon))
                    (unless (knownp tycon)
                      (error-parsing whole-expr "Unknown type constructor ~S" tycon))
                    ;; TODO: Make sure arity is correct!
                    (apply #'tyapp
                           (find-it tycon)
                           (mapcar #'parse args))))))

             (parse-constrained-type (expr)
               (multiple-value-bind (from to)
                   (parse-arrow 'coalton:=> (rest expr) ; exclude FOR symbol
                                :error (lambda ()
                                         (error-parsing whole-expr "Invalid constrained type ~
                                                                    because it lacks an ~
                                                                    arrow '=>': ~S" expr)))
                 (cond
                   ((null to) (error-parsing whole-expr "Constrained type requires a type to constrain: ~S" expr))
                   ((not (null (rest to))) (error-parsing whole-expr "Only one type can be constrained: ~S" expr)))

                 (if (endp from)
                     (parse (first to))
                     (make-cty (parse (first to)) :constraints (mapcar #'parse-constraint from)))))

             (parse-constraint (constraint)
               (unless (and (alexandria:proper-list-p constraint)
                            (= 2 (length constraint)))
                 (error-parsing whole-expr "Invalid constraint: ~S" constraint))
               (destructuring-bind (class-name-expr variable-expr) constraint
                 ;; Check that the class name is a symbol
                 (unless (symbolp class-name-expr)
                   (error-parsing whole-expr "Invalud constraint: ~S" constraint))
                 (let ((class-name class-name-expr)
                       (var        (parse variable-expr)))
                   (unless (typep var 'tyvar)
                     (error-parsing whole-expr "Invalid constraint: ~S" constraint))
                   (cx class-name var))))

             (parse (expr)
               (typecase expr
                 ;; TODO: Allow () for something useful?
                 (null
                  (error-parsing whole-expr "Invalid type expression: ~S" expr))

                 (symbol
                  (if (knownp expr)
                      (parse-nullary-constructor expr)
                      (parse-variable expr)))

                 (alexandria:proper-list
                  (parse-application expr))

                 (t
                  (error-parsing whole-expr "Invalid type expression: ~S" expr)))))
      ;; A constrained type can only be at the "top level".
      (values (if (and (alexandria:proper-list-p whole-expr)
                       (not (endp whole-expr))
                       (eq 'coalton:for (first whole-expr)))
                  (parse-constrained-type whole-expr)
                  (parse whole-expr))
              (alexandria:hash-table-alist table)))))
