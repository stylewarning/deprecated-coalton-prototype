;;;; toplevel-define-instance.lisp
;;;;
;;;; This file deals with parsing the DEFINE-CLASS top level form
;;;; and producing the corresponding class in the global dict

(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-INSTANCE.

#+this-is-an-example
(define-instance ((Eq a) => (Eq (Maybe a)))
  (define (== a b)
      (match a
        ((Just x) (match b
                    ((Just y) (== x y))
                    (Nothing  coalton:False)))
        (Nothing  (match b
                    ((Just _) coalton:False)
                    (Nothing  coalton:True))))))
#+this-is-an-example
(define-instance (Eq Boolean)
    (define (== a b)
      (or (and a       b)
          (and (not a) (not b)))))

(defun parse-instance-definiton (form)
  (destructuring-bind (def-instance class &rest methods) form
    (assert (eql 'coalton:define-instance def-instance))
    (assert (not (null class)))

    ;; TODO: We will need to add constraints/superclasses
    (multiple-value-bind (parsed-class parsed-type) (parse-instance-type class)
      (values parsed-class parsed-type
              (mapcar (lambda (x) (multiple-value-list (parse-instance-method x))) methods)))))

(defun parse-instance-type (form)
  ;; So here we can allow for type variables in the type only if that
  ;; type is constrained (superclass)

  (let ((form-contains-constraint? (eq 'coalton:for (first form))))
    (if form-contains-constraint?
        (progn
          (multiple-value-bind (from to)
              (parse-arrow 'coalton:=> (rest form)
                           :error (lambda ()
                                    (error-parsing form "Invalid constrained type ~
                                                 because it lacks an ~
                                                 arrow '=>': ~S" form)))
            ;; TODO: Maybe this should be parse-type-constraint?
            (let* ((parsing-context (make-type-parsing-context))
                   (constraints (mapcar (lambda (x) (parse-type-constraint x parsing-context)) from))
                   (class (first (first to)))
                   (types (mapcar (lambda (x) (parse-type x parsing-context)) (rest (first to))))
                   ;; XXX: This only allows for type variables to be
                   ;; constrained directly. This might not be
                   ;; desirable in more complex class instances. This
                   ;; could be fixed in the constraint filtering
                   ;; lambda.
                   (constrained-types
                     (mapcar (lambda (type)
                               (cty type
                                    :constraints
                                    (remove-if-not (lambda (cx)
                                                     (equal (cx-type cx)
                                                            type))
                                                   constraints)))
                             types)))
              (values class constrained-types))))
        (destructuring-bind (class &rest types) form
          ;; TODO: Do we need the type to be a non-function?
          (values class (mapcar #'parse-type-expression types))))))

(defun parse-instance-method (form)
  (destructuring-bind (define name/params &rest body) form
    (assert (eql 'coalton:define define))

    (values (first name/params)
            (parse-form
             `(coalton:fn ,(rest name/params) ,@body)))))

(defun compile-instance-method (implementation-symbol expr)
  `(define-global-lexical ,implementation-symbol
       ,(compile-value-to-lisp expr)))

(defun process-toplevel-instance-definitions (definstance-forms)
  (let ((compiled-forms nil))
    (dolist (form definstance-forms)
      (multiple-value-bind (class-name types methods) (parse-instance-definiton form)
        (unless (class-knownp class-name)
          (error-parsing form "Unknown type class ~S" class-name))

        (let* ((class (find-type-class class-name))
               (class-variables (type-class-variables class))
               (class-methods (type-class-methods class)))
          ;; Ensure instance is of the correct arity
          (unless (= (length class-variables)
                     (length types))
            (error-parsing form "Attempting to define instance of class with wrong arity. Wanted ~D but got ~D" (length class-variables) (length types)))

          ;; TODO: Check for errors in unification
          ;; (mapcar #'unify unified-types types)
          ;; (format t "UNIFIED ~A~%" unified-types)

          ;; Check that we have a complete definition of the class and
          ;; there are no extra methods defined.  In this context, a
          ;; full definition is one which contains all of the methods.
          (let ((method-names (mapcar #'first methods))
                (class-method-names (alexandria:hash-table-keys class-methods)))
            (let ((undefined-methods (set-difference class-method-names method-names))
                  (extra-methods (set-difference method-names class-method-names)))
              (unless (null extra-methods)
                (error-parsing form "Unknown method~P ~{~S~^, ~}" (length extra-methods) extra-methods))
              (unless (null undefined-methods)
                (error-parsing form "Missing method definition~P ~{~S~^, ~}" (length undefined-methods) undefined-methods))))

          (let ((parsed-methods (make-hash-table)))
            ;; Look up each method definiton, making sure it exists in the class
            ;; TODO: Check for complete definition of class (with no extras)
            (dolist (method methods)
              (let* ((method-name (first method))
                     (method-body (second method))
                     (class-method-definition (gethash method-name class-methods)))
                (unless class-method-definition
                  (error-parsing form "Unknown type class method ~S" method-name))

                ;; Here we need to replace all tyvars with the
                ;; corresponding type in the instance signature then
                ;; check that the type is equal (or possibly subtype)
                (let* ((fresh-type (fresh (derive-type method-body)))
                       (replacement-dict (mapcar #'cons class-variables types))

                       (class-type-with-replaced-types (replace-type-variables class-method-definition replacement-dict)))

                  ;; I think here we need to unify but something doesn't feel right about this
                  ;;TODO: We should catch unification errors and report type mismatches in a way the user can read
                  ;; FIXME: This does not actually modify the form.
                  (with-parsing-context (form)
                    (unify fresh-type class-type-with-replaced-types))

                  ;; Well, method looks good to me. Let's add it.
                  (let ((implementation-symbol
                          (generate-instance-method-symbol
                           class-name
                           (mapcar #'unparse-type types)
                           method-name)))
                    ;; Add the method implementation to the instance
                    (setf (gethash method-name parsed-methods)
                          (make-type-class-instance-method :name method-name
                                                           :type fresh-type
                                                           :implementation implementation-symbol))
                    ;; Emit a definition for the method
                    (push (compile-instance-method implementation-symbol method-body)
                          compiled-forms)

                    
                    ))))

            ;; Okay, so we made it this far. This (hopefully) means
            ;; that the instance definition is correct.
            ;; Let's add it to the class

            ;; TODO: Warn on overriding definitions
            (setf (gethash types (type-class-instances class))
                  (make-type-class-instance :types types
                                            :methods parsed-methods))))))
    ;; Return the compiled forms we got
    compiled-forms))
