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
          ':variable
          nil))

(defun parse-define-form-function (fvar args val)
  (check-type fvar symbol)
  ;; The (DEFINE (<fvar> . <args>) <val>) case.
  (values fvar
          (parse-form `(coalton:fn ,args ,val))
          ':function
          args))

;;; TODO: make sure we can lexically shadow global bindings
(defun compile-toplevel-define (name self expr kind args &optional (whole nil whole-provided-p))
  ;; WHOLE = the source form
  ;; NAME = var name being bound
  ;; SELF = is NAME self-referential in EXPR?
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
     (let (;;(inferred-type (node-derived-type expr))
           (internal-name (entry-internal-name (var-info name))))
       ;; FIXME check VAR-DECLARED-TYPE
       ;; FIXME check VAR-DERIVED-TYPE
       (setf ;;(var-derived-type name)             inferred-type
             (entry-node (var-info name))        expr)
       (when whole-provided-p
         (setf (entry-source-form (var-info name)) whole))
       (list*
        `(define-symbol-macro ,name ,internal-name)
        `(global-vars:define-global-var ,internal-name
             ,(if (not self)
                  (compile-value-to-lisp expr)
                  `(let (,name)
                    (setf ,name ,(compile-value-to-lisp expr))
                    ,name)))
        (when (eq ':function kind)
          (list
           `(defun ,name (,@args)
              (funcall ,name ,@args)))))))))

(defun process-toplevel-value-definitions (def-forms)
  (let* ((parsed (loop :for form :in def-forms
                       :collect (append (multiple-value-list (parse-define-form form))
                                        (list form))))
         (vars (mapcar #'first parsed))
         (vals (mapcar #'second parsed)))
    (multiple-value-bind (sorted cyclic self-referential) (sort-letrec-bindings vars vals)
      (setf cyclic (mapcar #'first cyclic))

      ;; We infer the types of the sorted list of variables. We do
      ;; this for effect: each node will be tagged with the type in the
      ;; DERIVED-TYPE slot.
      (loop :for var :in sorted ;(reverse sorted)
            :for raw-val := (second (assoc var parsed))
            :for val := (if (member var self-referential)
                            (node-letrec (list (cons var raw-val))
                                         (node-variable var))
                            raw-val)
            ;; Clobbering......
            :do (unless (var-knownp var)
                  (forward-declare-variable var))
                (setf (var-derived-type var) (derive-type val)))

      ;; Now we build up our expression. The center of our expression
      ;; will be an opaque value with a type variable. It's a bit of a
      ;; hack, but it's to avoid re-writing inference rules.  Around
      ;; it we form a LETREC out of our cyclic variables.
      (unless (null cyclic)
       (let ((expr (node-letrec
                    (loop :for var :in cyclic
                          :for val := (second (assoc var parsed))
                          :collect (cons var val))
                    (node-lisp (make-variable) nil))))
         ;; Next, we derive the type of the letrec expression.
         (derive-type expr)))

      ;; Now, we unpack all of the information. Do so in a sensible order.
      (append
       (loop :for var :in (append sorted cyclic)
             :for self-ref := (member var self-referential)
             :for (_ val kind args whole) := (assoc var parsed)
             ;; TODO: add the source form
             :append (compile-toplevel-define var self-ref val kind args whole))
       ;; NIL is just there for clean kicks
       '(nil)))))
