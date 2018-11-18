;;;; toplevel-define-type.lisp

(in-package #:coalton-impl)

;;; Handling of toplevel COALTON:DEFINE-TYPE.

(defun parse-define-type-form (form)
  (destructuring-bind (def-type type &rest ctors) form
    (assert (eql 'coalton:define-type def-type))
    (assert (not (null type)))
    (setf type (alexandria:ensure-list type))
    (destructuring-bind (tycon-name &rest tyvar-names) type
      (when (tycon-knownp tycon-name)
        (cerror "Clobber the tycon." "Already defined tycon: ~S" tycon-name))
      (assert (every #'symbolp tyvar-names))
      (let* ((arity (length tyvar-names))
             (tycon (make-tycon :name tycon-name :arity arity))
             (constructors nil))
        (multiple-value-bind (ty fvs) (parse-type-expression type :extra-tycons (list tycon))
          (dolist (ctor ctors)
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
                                              :extra-tycons (list tycon)
                                              :variable-assignments fvs))
                              ty))
                       constructors)))))
          (values tycon ty constructors))))))

(defun compile-toplevel-define-type-form (whole)
  (multiple-value-bind (tycon generic-ty ctors) (parse-define-type-form whole)
    (let* ((tycon-name (tycon-name tycon))
           (ctor-names (mapcar #'second ctors))
           (pred-names (loop :for ctor-name :in ctor-names
                             :collect (alexandria:format-symbol nil "~A-P" ctor-name))))
      ;; Record the ctors and predicates.
      (setf (tycon-constructors tycon) (mapcar #'cons ctor-names pred-names))

      ;; Make the tycon known. We clobber it if it exists.
      (setf (find-tycon tycon-name) tycon)

      ;; Declare the types of the new things.
      (loop :for (_ name ty) :in ctors
            :do (unless (var-knownp name)
                  (forward-declare-variable name))
                (setf (var-declared-type name) ty))

      ;; Declare the predicates
      (loop :with pred-ty := (make-function-type generic-ty boolean-type)
            :for (_ . pred-name) :in (tycon-constructors tycon)
            :do (unless (var-knownp pred-name)
                  (forward-declare-variable pred-name))
                (setf (var-declared-type pred-name) pred-ty))
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
         ;; Define predicates
         ,@(loop :for (ctor-name . pred-name) :in (tycon-constructors tycon)
                 :collect `(defun ,pred-name (object)
                             (typep object ',ctor-name)))
         ',tycon-name))))

(defun process-toplevel-type-definitions (deftype-forms)
  ;; TODO: Allow mutually recursive types.
  (mapcar #'compile-toplevel-define-type-form deftype-forms))
