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
                                           coalton:define-class
                                           coalton:define-instance
                                           coalton:declare))

;;; Entry Point

(defun collect-toplevel-forms (forms)
  "Walk through the top-level forms and sort them out. Return an alist from special operators to their forms."
  (let ((table (make-hash-table)))
    (labels ((flatten (forms)
               (loop :for form :in forms
                     :append (cond
                               ((atom form) (list form))
                               ((member (first form) **toplevel-operators**)
                                (flatten (rest form)))
                               (t (list form)))))
             (walk (forms)
               (if (endp forms)
                   (progn
                     (maphash (lambda (key val)
                                (setf (gethash key table) (nreverse val)))
                              table)
                     table)
                   (let ((next-form (first forms)))
                     (cond
                       ;; Error on invalid top level operators
                       ((or (atom next-form)
                            (not (member (first next-form) **special-operators**)))
                        (error-parsing next-form "This can't show up at the top-level."))
                       ;; Grab any special operators
                       ((member (first next-form) **special-operators**)
                        (push next-form (gethash (first next-form) table))
                        (walk (rest forms)))
                       ;; Oops, just errors!
                       (t
                        (assert nil () "Unreachable.")))))))
      (walk (flatten forms)))))

;;; Coalton Macros

(defmacro coalton:coalton-toplevel (&body toplevel-forms)
  "Top-level definitions for use within Coalton."
  (let ((form-table (collect-toplevel-forms toplevel-forms)))
    ;; We must process these in this order!
    `(progn
       ,@(process-toplevel-type-definitions (gethash 'coalton:define-type form-table))
       ,@(process-toplevel-class-definitions (gethash 'coalton:define-class form-table))
       ,@(process-toplevel-instance-definitions (gethash 'coalton:define-instance form-table))
       ,@(process-toplevel-declarations (gethash 'coalton:declare form-table))
       ,@(process-toplevel-value-definitions (gethash 'coalton:define form-table)))))

(defmacro coalton:coalton (coalton-form)
  "The bridge from Coalton to Lisp. Compute a Coalton value as a Lisp value."
  (let* ((parsed-form (parse-form coalton-form))
         (derived-type (derive-type parsed-form)))
    (declare (ignore derived-type))
    ;; TODO: We could do more type checking here.
    (compile-value-to-lisp parsed-form)))
