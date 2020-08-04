;;;; tycon-database.lisp

(in-package #:coalton-impl)

;;; TODO: figure out type aliases.
(define-global-var **type-definitions** (make-hash-table :test 'eql)
  "Database of Coalton type definitions. These are mappings from symbols to type constructors.")

(defmacro define-type-constructor (name arity)
  "Globally define a type constructor named NAME (a symbol) with arity ARITY (a non-negative integer).

If NAME is already known (and the known arity matches), nothing will happen. If it doesn't match, an error will be signaled.

If NAME is not known, it will be made known to the global type database."
  (check-type name symbol)
  (check-type arity (integer 0))
  (alexandria:with-gensyms (entry exists?)
    `(multiple-value-bind (,entry ,exists?) (gethash ',name **type-definitions**)
       (cond
         (,exists?
          (when (/= ,arity (tycon-arity ,entry))
            (error "Trying to redefine tycon ~S with a different arity." ',name)))
         (t
          (setf (gethash ',name **type-definitions**)
                (make-tycon :name ',name :arity ',arity))
          ',name)))) )

(defun tycon-knownp (tycon-name)
  "Do we know of a tycon named TYCON-NAME?"
  (check-type tycon-name symbol)
  (nth-value 1 (gethash tycon-name **type-definitions**)))

(defun find-tycon (tycon-name)
  (check-type tycon-name symbol)
  (or (gethash tycon-name **type-definitions**)
      (error "Couldn't find definition of tycon ~S" tycon-name)))

(defun (setf find-tycon) (new-value tycon-name)
  (check-type tycon-name symbol)
  (check-type new-value tycon)

  ;; Warn about clobbering a non-identical tycon, and invalidate the
  ;; old one.
  (when (tycon-knownp tycon-name)
    (let ((existing-tycon (find-tycon tycon-name)))
      (unless (eq existing-tycon new-value)
        (style-warn "Clobbering tycon ~S" tycon-name)
        (setf (tycon-invalidated existing-tycon) t))))

  (setf (gethash tycon-name **type-definitions**) new-value))

(defun find-tycon-for-ctor (name)
  (loop :for tycon-name :being :the :hash-keys :of **type-definitions**
          :using (hash-value tycon)
        :when (find name (tycon-constructors tycon))
          :do (return tycon)
        :finally (return nil)))
