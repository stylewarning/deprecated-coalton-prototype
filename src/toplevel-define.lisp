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
          (parse-form `(coalton:letrec ((,var ,val)) ,var))
          ':variable
          nil))

(defun parse-define-form-function (fvar args val)
  (check-type fvar symbol)
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (values fvar
          (parse-form `(coalton:letrec ((,fvar  (coalton:fn ,args ,val))) ,fvar))
          ':function
          args))

;;; TODO: make sure we can lexically shadow global bindings
(defun compile-toplevel-define (name expr kind args &optional (whole nil whole-provided-p))
  ;; WHOLE = the source form
  ;; NAME = var name being bound
  ;; EXPR = a NODE instance whose type has already been derived.
  ;; KIND = {:variable, :function}
  ;; ARGS = a list of arguments if the KIND if a :FUNCTION
  (cond
    ((var-definedp name)
     ;; XXX: Get this right. Re-typecheck everything?!
     (let ((internal-name (entry-internal-name (var-info name))))
       (list
        `(setf ,internal-name ,(compile-value-to-lisp expr)))))
    (t
     (unless (var-knownp name)
       ;; Declare the variable.
       (forward-declare-variable name))
     ;; The type inferencing has already been done by this point. (See
     ;; `PROCESS-TOPLEVEL-VALUE-DEFINITIONS'.)
     (let ((inferred-type (node-derived-type expr))
           (internal-name (entry-internal-name (var-info name))))
       ;; FIXME check VAR-DECLARED-TYPE
       ;; FIXME check VAR-DERIVED-TYPE
       (setf (var-derived-type name)             inferred-type
             (entry-node (var-info name))        expr)
       (when whole-provided-p
         (setf (entry-source-form (var-info name)) whole))
       (list*
        `(define-symbol-macro ,name ,internal-name)
        `(global-vars:define-global-var ,internal-name ,(compile-value-to-lisp expr))
        (when (eq ':function kind)
          (list
           `(defun ,name (,@args)
              (funcall ,name ,@args)))))))))

(defun process-toplevel-value-definitions (def-forms)
  (let* ((parsed (loop :for form :in def-forms
                       :collect (multiple-value-list (parse-define-form form))))
         (vars (mapcar #'first parsed))
         (vals (mapcar #'second parsed)))
    (multiple-value-bind (sorted cyclic) (sort-letrec-bindings vars vals)
      (setf cyclic (mapcar #'first cyclic))
      ;; Now we build up our expression. The center of our expression
      ;; will be an opaque value with a type variable. It's a bit of a
      ;; hack, but it's to avoid re-writing inference rules.  Around
      ;; it we form a LETREC out of our cyclic variables.
      (let ((expr (node-letrec
                    (loop :for var :in cyclic
                          :for val := (second (assoc var parsed))
                          :collect (cons var val))
                    (node-lisp (make-variable) nil))))
        ;; We infer the types of the sorted list of variables. We do
        ;; this for effect: each node will be tagged with the type in the
        ;; DERIVED-TYPE slot.
        (loop :for var :in (reverse sorted)
              :for val := (second (assoc var parsed))
              :for ty := (derive-type val)
              ;; Clobbering......
              :do (setf (var-derived-type var) ty))
        ;; Next, we derive the type of the letrec expression.
        (derive-type expr)
        ;; Now, we unpack all of the information. Do so in a sensible order.
        (append
         (loop :for var :in (append sorted cyclic)
               :for (_ val kind args) := (assoc var parsed)
               ;; TODO: add the source form
               :append (compile-toplevel-define var val kind args))
         ;; NIL is just there for clean kicks
         '(nil))))))
