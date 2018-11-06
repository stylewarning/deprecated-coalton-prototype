;;;; early-types.lisp

(in-package #:coalton-impl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-function-type (from to)
  "Make a function type mapping FROM to TO. Equivalent to (-> FROM TO)."
  (tyfun (alexandria:ensure-list from) to))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; INTEGER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Needed to work with integer literals.
(define-type-constructor coalton:integer 0)
(define-global-var* integer-type (tyapp (find-tycon 'coalton:integer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; STRING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Needed to work with string literals.
(define-type-constructor coalton:string 0)
(define-global-var* string-type (tyapp (find-tycon 'coalton:string)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BOOLEAN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is needed to process COALTON:IF

(define-type-constructor coalton:boolean 0)
(define-global-var* boolean-type (tyapp (find-tycon 'coalton:boolean)))
(forward-declare-variable 'coalton:True  boolean-type)
(forward-declare-variable 'coalton:False boolean-type)

;; (macroexpand-1 `(coalton:coalton
;;                   (coalton:define-type Boolean
;;                     True
;;                     False)))

(DEFCLASS COALTON:BOOLEAN NIL NIL
  (:METACLASS ABSTRACT-CLASS))
(DEFCLASS COALTON:FALSE (COALTON:BOOLEAN) NIL
  (:METACLASS SINGLETON-CLASS))
(DEFMETHOD PRINT-OBJECT ((SELF COALTON:FALSE) STREAM)
  (FORMAT STREAM "#.~s" 'COALTON:FALSE))
(DEFCLASS COALTON:TRUE (COALTON:BOOLEAN) NIL
  (:METACLASS SINGLETON-CLASS))
(DEFMETHOD PRINT-OBJECT ((SELF COALTON:TRUE) STREAM)
  (FORMAT STREAM "#.~s" 'COALTON:TRUE))
(GLOBAL-VARS:DEFINE-GLOBAL-VAR* COALTON:FALSE (MAKE-INSTANCE 'COALTON:FALSE))
(GLOBAL-VARS:DEFINE-GLOBAL-VAR* COALTON:TRUE (MAKE-INSTANCE 'COALTON:TRUE))


(declaim (inline lisp-boolean-to-coalton-boolean))
(defun lisp-boolean-to-coalton-boolean (x)
  (if x coalton:true coalton:false))

(declaim (inline falsity))
(defun falsity (x)
  (or (null x) (eql x coalton:false)))


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
