;;;; toplevel-define.lisp

(in-package #:coalton-impl)

;;; Handling of top-level COALTON:DEFINE.

(defun parse-define-form (form)
  "Parse a COALTON:DEFINE form."
  (check-compound-form form 'coalton:define)
  (check-compound-form-length form 3)
  ;; Defines either define a value or a function. Values and functions
  ;; in Coalton occupy the namespace, but the intent of the user can
  ;; be distinguished. A definition either looks like:
  ;;
  ;;     (DEFINE <var> <val>)
  ;;
  ;; or
  ;;
  ;;     (DEFINE (<fvar> <arg>*) <val>)
  ;;
  ;; The former defines a variable, the latter defines a function.
  (destructuring-bind (def-symbol var-thing val) form
    (declare (ignore def-symbol))
    (cond
      ((null var-thing)
       (error-parsing form "Found a null value where a symbol or function ~
                            was expected."))
      ((symbolp var-thing)
       (parse-define-form-variable var-thing val))
      ((and (listp var-thing)
            (every #'symbolp var-thing))
       (parse-define-form-function (first var-thing) (rest var-thing) val))
      (t
       (error-parsing form "Invalid second argument.")))))

(defun parse-define-form-variable (var val)
  ;; The (DEFINE <var> <val>) case.
  (check-type var symbol)
  ;; XXX: Should this be LETREC too? Probably for something like F = x => ... F.
  (values var
          (parse-form val)
          ':variable))

(defun parse-define-form-function (fvar args val)
  (check-type fvar symbol)
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (values fvar
          (parse-form
           `(coalton:letrec ((,fvar (coalton:fn ,args ,val)))
              ,fvar))
          ':function
          args))

;;; TODO: make sure we can lexically shadow global bindings
(defun compile-toplevel-define-form (whole)
  (multiple-value-bind (name expr kind &optional args) (parse-define-form whole)
    (cond
      ((var-definedp name)
       ;; XXX: Get this right. Re-typecheck everything?!
       (let ((internal-name (entry-internal-name (var-info name))))
         `(setf ,internal-name ,(compile-value-to-lisp expr))))
      (t
       (unless (var-knownp name)
         ;; Declare the variable.
         (forward-declare-variable name))
       ;; Do some type inferencing.
       (let ((inferred-type (derive-type expr))
             (internal-name (entry-internal-name (var-info name))))
         ;; FIXME check VAR-DECLARED-TYPE
         ;; FIXME check VAR-DERIVED-TYPE
         (setf (var-derived-type name)             inferred-type
               (entry-source-form (var-info name)) whole
               (entry-node (var-info name))        expr)
         `(progn
            (define-symbol-macro ,name ,internal-name)
            (global-vars:define-global-var ,internal-name ,(compile-value-to-lisp expr))
            ,@(when (eq ':function kind)
                (list
                 `(defun ,name (,@args)
                    (funcall ,name ,@args))))
            ',name))))))

(defun process-toplevel-value-definitions (def-forms)
  (mapcar #'compile-toplevel-define-form def-forms))
