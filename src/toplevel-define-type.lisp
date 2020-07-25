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
      (assert (every #'symbolp tyvar-names))
      (let* ((arity (length tyvar-names))
             (fresh-tycon (make-tycon :name tycon-name :arity arity))
             (tycon (if (tycon-knownp tycon-name)
                        (find-tycon tycon-name)
                        fresh-tycon)))
        ;; The point of this check is to see if the tycon we already
        ;; know is "compatible" with the one we are parsing. If it is,
        ;; we'll use the one we know. However, if the one we know is
        ;; inconsistent with the one we are parsing, we should error.
        ;;
        ;; TODO: Should we also verify the constructors here?
        (when (/= arity (tycon-arity tycon))
          (cerror "Clobber the tycon." "Tycon ~S already defined, but with different arity." tycon-name)
          (setf tycon fresh-tycon))
        (list* tycon type ctors)))))


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

;;; We have to create "mangled" names so we can support
;;; types defined like
;;;
;;;     (define-type A A)
;;;
;;; or
;;;
;;;     (define-type (Ref t) (Ref t))
;;;
;;; The code generation later in this file will use
;;; ASSEMBLE-CTOR-CLASS-NAME to produce class names for the
;;; constructor objects. In this case, we would produce A/A or REF/REF
;;; as the class names.
(defun assemble-ctor-class-name (tycon-name ctor-name)
  "Construct the (Lisp) class name corresponding to objects constructed by the constructor named CTOR-NAME. TYCON-NAME should be the name of the tycon that CTOR-NAME constructs."
  (check-type tycon-name symbol)
  (check-type ctor-name symbol)
  (alexandria:format-symbol (symbol-package tycon-name)
                            "~A/~A"
                            (symbol-name tycon-name)
                            (symbol-name ctor-name)))

;;; We need to understand how to compile these following cases:
;;;
;;;
;;; An empty type:
;;;
;;;     (define-type X)
;;;
;;; A singleton type:
;;;
;;;     (define-type X X)
;;;
;;; An enumeration:
;;;
;;;     (define-type X X Y Z)
;;;
;;; A type with a single non-parametric constructor:
;;;
;;;     (define-type X (X Integer))
;;;
;;; A type with a single parametric constructor:
;;;
;;;     (define-type (X t) (X t))
;;;
;;; A type with both a parametric constructor and constant:
;;;
;;;     (define-type (X t) (X t) Y)
;;;
;;; A type with several parametric constructors:
;;;
;;;     (define-type (X t) (X t) (Y t))
;;;
(defun compile-toplevel-define-type-form (tycon generic-ty ctors)
  (declare (ignore generic-ty))
  ;; TYCON: The type to define (a TYCON object).
  ;;
  ;; GENERIC-TY: The fully generic type object representing TYCON (a TY object).
  ;;
  ;; CTORS: The parsed constructors (see above).
  (let* ((tycon-name (tycon-name tycon))
         (ctor-names (mapcar #'second ctors)))
    ;; Record the ctors.
    ;;
    ;; XXX: This may clobber existing known ctor functions!
    (setf (tycon-constructors tycon) ctor-names)

    ;; Make the tycon known. We clobber it if it exists and is
    ;; inequivalent.
    ;;
    ;; TODO: Warn if the tycon name is suspicious (e.g., has a #\/
    ;; character, matches some other constructor name, etc.).
    (setf (find-tycon tycon-name) tycon)

    ;; Declare the types of the new things.
    (loop :for (_ name ty) :in ctors
          :do (unless (var-knownp name)
                (forward-declare-variable name))
              (setf (var-declared-type name) ty))

    ;; Compile into sensible Lisp. As an example, let's consider:
    ;;
    ;;     (define-type (A t) (A t) B)
    ;;
    ;; TODO: Structs? Vectors? Classes? This should be thought
    ;; about. Let's start with classes.
    `(progn
       ;; Define types. Create the superclass.
       ;;
       ;; Continuing the example, we'll generate the following
       ;; abstract class:
       ;;
       ;;     (DEFCLASS A ())
       ;;
       (defclass ,tycon-name ()
         ()
         (:metaclass abstract-class))

       ;; Create all of the subclasses.
       ;;
       ;; Continuing the example, we have constructors A and B. We
       ;; will generate new classes for each of these
       ;; constructors. The convention will be that the constructor
       ;; names are <tycon-name>/<ctor-name>. This has a only very
       ;; mild unfortunate consequence that somebody may have used
       ;; such a name for a different tycon, but that's so rare and
       ;; bad that we just nix it as a possibility.
       ;;
       ;; In this case, we generate
       ;;
       ;;     (DEFCLASS A/A (A))
       ;;     (DEFCLASS A/B (A))
       ;;
       ;; These classes will either be singleton or final, depending
       ;; on whether they're parametric or not.
       ,@(loop :for (kind name _) :in ctors
               :for class-name := (assemble-ctor-class-name tycon-name name)
               :collect (ecase kind
                          (:variable
                           `(defclass ,class-name (,tycon-name)
                              ()
                              (:metaclass singleton-class)))
                          (:function
                           `(defclass ,class-name (,tycon-name)
                              ;; XXX: For now, we just store a vector.
                              ((value :initarg :value
                                      :type simple-vector))
                              (:metaclass final-class))))
               ;; We generate PRINT-OBJECT methods for these.
               ;;
               ;; TODO: Handle readability properly.
               :collect (ecase kind
                          (:variable
                           `(defmethod print-object ((self ,class-name) stream)
                              (format stream "#.~s" ',name)))
                          (:function
                           `(defmethod print-object ((self ,class-name) stream)
                              (format stream "#.(~s~{ ~s~})"
                                      ',name
                                      (coerce (slot-value self 'value) 'list))))))

       ;; Define constructors.
       ;;
       ;; There are two possibilities: the constructor is a function
       ;; or a variable.
       ;;
       ;; Continuing the example above, we have (A t) and B.
       ;;
       ;; Function case:
       ;;
       ;;     (DEFUN A (X) (MAKE-INSTANCE 'A/A :VALUE (VECTOR X)))
       ;;     (DEFINE-GLOBAL-VAR* A #'A)
       ;;
       ;; Variable case:
       ;;
       ;;     (DEFINE-GLOBAL-VAR* B (MAKE-INSTANCE 'A/B))
       ,@(loop :for (kind name ty) :in ctors
               :for class-name := (assemble-ctor-class-name tycon-name name)
               :append (ecase kind
                         ;; TODO: Should we emulate a global
                         ;; lexical? The type inference assumes as
                         ;; much.
                         (:variable
                          (list
                           `(define-global-var* ,name (make-instance ',class-name))))
                         (:function
                          (let* ((arity (tyfun-arity ty))
                                 (args (loop :repeat arity
                                             :collect (gensym "A"))))
                            (list
                             `(defun ,name ,args
                                (make-instance ',class-name :value (vector ,@args)))
                             ;; FIXME: make lexical
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
