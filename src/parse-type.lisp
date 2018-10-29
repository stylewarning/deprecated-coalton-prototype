;;;; parse-type.lisp

(in-package #:coalton-impl)

;;; Grammar:
;;;
;;;    <type expr> := <type alias>                       ; TODO!
;;;                 | <type variable>
;;;                 | <nullary type constructor>
;;;                 | (<type constructor> <type expr>*)

(defun parse-type-expression (whole-expr)
  "Parse the type expression WHOLE-EXPR. Return two values:

1. The parsed expression.

2. An a-list of symbol -> TYVAR pairs.
"
  ;; Below, TABLE is a mapping from symbols to fresh type variables.
  (let ((table (make-hash-table)))
    (labels ((parse-variable (expr)
               (check-type expr symbol)
               (or (gethash expr table)
                   (setf (gethash expr table) (make-variable))))

             (parse-nullary-constructor (expr)
               (check-type expr symbol)
               (unless (tycon-knownp expr)
                 (error-parsing whole-expr "Unknown type constructor ~S" expr))
               (tyapp (find-tycon expr)))

             (parse-application (expr)
               (destructuring-bind (tycon &rest args) expr
                 (unless (symbolp tycon)
                   (error-parsing whole-expr "Invalid part of type expression: ~S" tycon))
                 (unless (tycon-knownp tycon)
                   (error-parsing whole-expr "Unknown type constructor ~S" tycon))
                 (apply #'tyapp
                        (find-tycon tycon)
                        (mapcar #'parse args))))

             (parse (expr)
               (typecase expr
                 ;; TODO: Allow () for something useful?
                 (null
                  (error-parsing whole-expr "Invalid type expression: ~S" expr))

                 (symbol
                  (if (tycon-knownp expr)
                      (parse-nullary-constructor expr)
                      (parse-variable expr)))

                 (alexandria:proper-list
                  (parse-application expr))

                 (t
                  (error-parsing whole-expr "Invalid type expression: ~S" expr)))))
      (values (parse whole-expr)
              (alexandria:hash-table-alist table)))))
