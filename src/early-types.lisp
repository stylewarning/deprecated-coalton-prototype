;;;; early-types.lisp

(in-package #:coalton-impl)

(define-type-constructor coalton:-> 2)

(defun make-function-type (from to)
  "Make a function type mapping FROM to TO. Equivalent to (-> FROM TO)."
  (tyfun (alexandria:ensure-list from) to))

(define-type-constructor coalton:integer 0)
(define-global-var* integer-type (tyapp (find-tycon 'coalton:integer)))

(define-type-constructor coalton:boolean 0)
(define-global-var* boolean-type (tyapp (find-tycon 'coalton:boolean)))

(define-type-constructor coalton:unit 0)
(define-global-var* unit-type (tyapp (find-tycon 'coalton:unit)))
