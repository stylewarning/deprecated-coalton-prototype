;;;; early-types.lisp

(in-package #:coalton-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-function-type (from to)
  "Make a function type mapping FROM to TO. Equivalent to (FN FROM -> TO)."
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
(unless (var-knownp 'coalton:Unit)
  (forward-declare-variable 'coalton:Unit unit-type))

;; (macroexpand-1 `(coalton:coalton-toplevel
;;                   (coalton:define-type Unit
;;                     Unit)))

(DEFCLASS COALTON:UNIT () ()
  (:METACLASS ABSTRACT-CLASS))
(DEFCLASS COALTON::UNIT/UNIT (COALTON:UNIT) ()
  (:METACLASS SINGLETON-CLASS))
(DEFMETHOD PRINT-OBJECT ((SELF COALTON::UNIT/UNIT) STREAM)
  (FORMAT STREAM "#.~s" 'COALTON:UNIT))
;;; FIXME: make this lexical
(DEFINE-GLOBAL-LEXICAL COALTON:UNIT (MAKE-INSTANCE 'COALTON::UNIT/UNIT))
