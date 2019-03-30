;;;; early-types.lisp

(in-package #:coalton-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-function-type (from to)
  "Make a function type mapping FROM to TO. Equivalent to (-> FROM TO)."
  (tyfun (alexandria:ensure-list from) to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; VOID ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Needed for pattern matching with no clauses.
(define-type-constructor coalton:void 0)
(define-global-var* void-type (tyapp (find-tycon 'coalton:void)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTEGER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Needed to work with integer literals.
(define-type-constructor coalton:integer 0)
(define-global-var* integer-type (tyapp (find-tycon 'coalton:integer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Needed to work with string literals.
(define-type-constructor coalton:string 0)
(define-global-var* string-type (tyapp (find-tycon 'coalton:string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UNIT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is needed to process COALTON:PROGN

(define-type-constructor coalton:unit 0)
(define-global-var* unit-type (tyapp (find-tycon 'coalton:unit)))
(forward-declare-variable 'coalton:Singleton unit-type)

;; (macroexpand-1 `(coalton:coalton
;;                   (coalton:define-type Unit
;;                     Singleton)))

(DEFCLASS COALTON:UNIT NIL NIL
  (:METACLASS ABSTRACT-CLASS))
(DEFCLASS COALTON:SINGLETON (COALTON:UNIT) NIL
  (:METACLASS SINGLETON-CLASS))
(DEFMETHOD PRINT-OBJECT ((SELF COALTON:SINGLETON) STREAM)
  (FORMAT STREAM "#.~s" 'COALTON:SINGLETON))
(GLOBAL-VARS:DEFINE-GLOBAL-VAR* COALTON:SINGLETON (MAKE-INSTANCE 'COALTON:SINGLETON))
