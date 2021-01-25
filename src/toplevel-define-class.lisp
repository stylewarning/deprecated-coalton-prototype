;;;; toplevel-define-class.lisp
;;;;
;;;; This file deals with parsing the DEFINE-CLASS top level form
;;;; and producing the corresponding class in the global dict

(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-CLASS.

(defun parse-class-definition (form)
  "Parses a type class definiton of the form

(DEFINE-CLASS (NAME a1 a2 ...)
  (method1 type1)
  (method2 type2)
  ...)

and returns

(VALUES CLASS-NAME PARSED-VARS PARSED-METHODS)"
  (let (variable-assignments)
    ;; TODO: This should just use constraint parsing with a global env
    ;; for the class definition
    
    (labels ((parse-class-method (form)
               ;; Extract method name and provided type expression
               (destructuring-bind (method-name type-expr) form
                 (assert (symbolp method-name))
                 ;; Parse the type expr, disallowing new type variables

                 ;; TODO: Catch parse errors on types and throw a more specific error
                 (let ((parsed-type (parse-type-expression type-expr
                                                           :variable-assignments variable-assignments
                                                           :allow-new-variables nil)))
                   (values method-name parsed-type)))))

      ;; Extract parts of class definiton
      (destructuring-bind (def-class class &rest methods) form
        (assert (eql 'coalton:define-class def-class))
        (assert (not (null class)))
        
        ;; TODO: This does not allow for naked type names. This might not be desirable.
        (destructuring-bind (class-name &rest var-names) class
          (assert (symbolp class-name))
          (assert (every #'symbolp var-names))
          
          ;; Assign type variables
          (dolist (var-name var-names)
            (let* ((tyvar (make-variable var-name))
                   (cxvar (cx class-name tyvar))
                   (ctyvar (cty tyvar :constraints (list cxvar))))
              (push (cons var-name ctyvar)
                    variable-assignments)))

          ;; Parse methods
          (let ((parsed-methods (make-hash-table)))
            (dolist (method methods)
              (multiple-value-bind (method-name method-type)
                  (parse-class-method method)
                ;; Disallow duplicate method names
                (when (gethash method-name parsed-methods)
                  (error-parsing form "Duplicate method definiton for ~S" method-name))

                ;; Update the class definition with this method
                (setf (gethash method-name parsed-methods)
                      (normalize-type method-type))

                ;; TODO: Don't allow defining methods for
                ;; already-defined functions, however allow for
                ;; redefining of methods in the same class. hmm
                ))
            (let ((parsed-type-vars (mapcar #'cty-type (mapcar #'cdr variable-assignments))))
              (values class-name parsed-type-vars parsed-methods))))))))

(defun process-toplevel-class-definitions (defclass-forms)
  (dolist (form defclass-forms)
    (multiple-value-bind (class-name type-vars parsed-methods)
        (parse-class-definition form)
      ;; Put the new class definition on the dict
      (multiple-value-bind (entry exists?) (gethash class-name **class-definitions**)
        (when exists?
          (cerror "Override class definition" "Attempting to redefine class ~S." class-name))
        (setf (gethash class-name **class-definitions**)
              (make-type-class :name class-name
                               :variables type-vars
                               :methods parsed-methods))
        
        ;; Declare types for all methods
        (maphash (lambda (method-name method-type)
                   ;; Declare the type of this method
                   (unless (var-knownp method-name)
                     (forward-declare-variable method-name))
                   (setf (var-declared-type method-name) method-type)

                   (setf (gethash method-name **method-definitions**)
                         class-name))
                 parsed-methods)))))
