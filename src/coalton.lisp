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
                                            coalton:coalton-toplevel))
(define-global-var **special-operators** `(,@**toplevel-operators**
                                           coalton:define
                                           coalton:define-type
                                           coalton:declare))

;;; Entry Point

(defun collect-toplevel-forms (forms)
  "Walk through the top-level forms and sort them out. Return three values:

    1. All DEFINE-TYPE forms

    2. All DECLARE forms

    3. All DEFINE forms.
"
  (labels ((flatten (forms)
             (loop :for form :in forms
                   :append (cond
                             ((atom form) (list form))
                             ((member (first form) **toplevel-operators**)
                              (flatten (rest form)))
                             (t (list form)))))
           (walk (forms deftypes declares defines)
             (if (endp forms)
                 (values (nreverse deftypes) (nreverse declares) (nreverse defines))
                 (let ((next-form (first forms)))
                   (cond
                     ((or (atom next-form)
                          (not (member (first next-form) **special-operators**)))
                      (error-parsing next-form "This can't show up at the top-level."))
                     ((eql 'coalton:define-type (first next-form))
                      (walk (rest forms)
                            (cons next-form deftypes)
                            declares
                            defines))
                     ((eql 'coalton:declare (first next-form))
                      (walk (rest forms)
                            deftypes
                            (cons next-form declares)
                            defines))
                     ((eql 'coalton:define (first next-form))
                      (walk (rest forms)
                            deftypes
                            declares
                            (cons next-form defines)))
                     (t
                      (assert nil () "Unreachable.")))))))
    (walk (flatten forms) nil nil nil)))

;;; Coalton Macros

(defmacro coalton:coalton-toplevel (&body toplevel-forms)
  "Top-level definitions for use within Coalton."
  (multiple-value-bind (deftypes declares defines)
      (collect-toplevel-forms toplevel-forms)
    ;; We must process these in this order!
    `(progn
       ,@(process-toplevel-type-definitions deftypes)
       ,@(process-toplevel-declarations declares)
       ,@(process-toplevel-value-definitions defines))))

(defmacro coalton:coalton (coalton-form)
  "The bridge from Coalton to Lisp. Compute a Coalton value as a Lisp value."
  (let* ((parsed-form (parse-form coalton-form))
         (derived-type (derive-type parsed-form)))
    (declare (ignore derived-type))
    ;; TODO: We could do more type checking here.
    (compile-value-to-lisp parsed-form)))
