;;;; toplevel-define-type.lisp

(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-TYPE.

(defun parse-out-tycon (form)
  "Return a list:

    (TYCON UNPARSED-TYPE . CTORS)

where

    TYCON is the new type constructor

    UNPARSED-TYPE is the raw type expression with unchanged bound variables

    CTORS are the raw constructor expressions."
  (destructuring-bind (def-type type &rest ctors) form
    (assert (eql 'coalton:define-type def-type))
    (assert (not (null type)))
    (setf type (alexandria:ensure-list type))
    (destructuring-bind (tycon-name &rest tyvar-names) type
      (assert (symbolp tycon-name))
      (when (tycon-knownp tycon-name)
        (cerror "Clobber the tycon." "Already defined tycon: ~S" tycon-name))
      (assert (every #'symbolp tyvar-names))
      (list* (make-tycon :name tycon-name :arity (length tyvar-names))
             type
             ctors))))


(defun parse-out-ctors (this-tycon raw-type raw-ctors extra-tycons)
  ;; Inputs:
  ;;
  ;;     THIS-TYCON: A TYCON whose constructors we are parsing.
  ;;
  ;;     RAW-TYPE: The raw/unparsed type expression (a list).
  ;;
  ;;     RAW-CTORS: The raw constructors that have not been parsed.
  ;;
  ;;     EXTRA-TYCONS: Extra (usually mutually recursive) TYCONs that
  ;;                   should be known about.
  ;;
  ;; Outputs:
  ;;
  ;;     THIS-TYCON: The same as the input.
  ;;
  ;;     TY: A parsed, generic type object representing TYCON.
  ;;
  ;;     CTORS: Parsed constructors of the form
  ;;
  ;;              (:VARIABLE CTOR-NAME TYPE)
  ;;              (:FUNCTION CTOR-NAME TYPE)
  (multiple-value-bind (ty fvs)
      ;; When parsing the RAW-TYPE, we only want to augment the type
      ;; environment with THIS-TYCON, not all of them.
      (parse-type-expression raw-type :extra-tycons (list this-tycon))
    (let ((constructors nil))
      (dolist (ctor raw-ctors)
        (typecase ctor
          (symbol
           (push (list ':variable ctor ty) constructors))
          (alexandria:proper-list
           (destructuring-bind (name &rest argtys) ctor
             (push (list ':function
                         name
                         (make-function-type
                          (loop :for argty :in argtys
                                :collect (parse-type-expression
                                          argty
                                          :extra-tycons extra-tycons
                                          :variable-assignments fvs))
                          ty))
                   constructors)))))
      (values this-tycon ty (reverse constructors)))))

(defun compile-toplevel-define-type-form (tycon generic-ty ctors)
  ;; TYCON: The type to define (a TYCON object).
  ;;
  ;; GENERIC-TY: The fully generic type object representing TYCON (a TY object).
  ;;
  ;; CTORS: The parsed constructors (see above).
  (let* ((tycon-name (tycon-name tycon))
         (ctor-names (mapcar #'second ctors)))
    ;; Record the ctors.
    (setf (tycon-constructors tycon) ctor-names)

    ;; Make the tycon known. We clobber it if it exists.
    (setf (find-tycon tycon-name) tycon)

    ;; Declare the types of the new things.
    (loop :for (_ name ty) :in ctors
          :do (unless (var-knownp name)
                (forward-declare-variable name))
              (setf (var-declared-type name) ty))

    ;; Compile into sensible Lisp.
    ;;
    ;; TODO: Structs? Vectors? Classes? This should be thought
    ;; about. Let's start with classes.
    `(progn
       ;; Define types. Create the superclass.
       ;;
       ;; TODO: handle special case of 1 ctor.
       ,(if (endp ctors)
            `(deftype ,tycon-name () nil)
            `(defclass ,tycon-name ()
               ()
               (:metaclass abstract-class)))

       ;; Create all of the subclasses.
       ,@(loop :for (kind name _) :in ctors
               :collect (ecase kind
                          (:variable
                           `(defclass ,name (,tycon-name)
                              ()
                              (:metaclass singleton-class)))
                          (:function
                           `(defclass ,name (,tycon-name)
                              ;; XXX: For now, we just store a vector.
                              ((value :initarg :value
                                      :type simple-vector))
                              (:metaclass final-class))))
               :collect (ecase kind
                          (:variable
                           `(defmethod print-object ((self ,name) stream)
                              (format stream "#.~s" ',name)))
                          (:function
                           `(defmethod print-object ((self ,name) stream)
                              (format stream "#.(~s~{ ~s~})"
                                      ',name
                                      (coerce (slot-value self 'value) 'list))))))

       ;; Define constructors
       ,@(loop :for (kind name ty) :in ctors
               :append (ecase kind
                         ;; TODO: Should we emulate a global
                         ;; lexical? The type inference assumes as
                         ;; much.
                         (:variable
                          (list
                           `(define-global-var* ,name (make-instance ',name))))
                         (:function
                          (let* ((arity (tyfun-arity ty))
                                 (args (loop :repeat arity
                                             :collect (gensym "A"))))
                            (list
                             `(defun ,name ,args
                                (make-instance ',name :value (vector ,@args)))
                             `(define-global-var* ,name #',name))))))
       ',tycon-name)))

(defun process-toplevel-type-definitions (deftype-forms)
  ;; We can't go through types one-by-one because then we wouldn't set
  ;; things up nicely to allow recursion. We need to extract all the
  ;; names first before we can continue compiling.
  (let* ((tycon/ctors (mapcar #'parse-out-tycon deftype-forms))
         (tycons (mapcar #'first tycon/ctors)))
    (loop :for (tycon raw-ty . raw-ctors) :in tycon/ctors
          :collect 
          (multiple-value-call #'compile-toplevel-define-type-form
            (parse-out-ctors tycon raw-ty raw-ctors tycons)))))
