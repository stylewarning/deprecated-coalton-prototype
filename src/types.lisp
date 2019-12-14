;;;; types.lisp

(in-package #:coalton-impl)

;;; A type constructor is not a constructor for a value, but a
;;; constructor for a *type*! Get with the program!
(defstruct tycon
  "A constructor for type applications."
  (name (required 'name) :type symbol                 :read-only t)
  (arity 0               :type unsigned-byte          :read-only t)
  ;; A list of CONSTRUCTOR-NAMEs.
  ;;
  ;; The CONSTRUCTOR-NAME names a known function that constructs a
  ;; value of TYCON type.
  ;;
  ;; This isn't read-only because we might set it later. It would be
  ;; nice to make it read-only though.
  (constructors nil      :type alexandria:proper-list))

(make-load-form-for-struct tycon)

;;; TODO: figure out type aliases.
(define-global-var **type-definitions** (make-hash-table :test 'eql)
  "Database of Coalton type definitions. These are mappings from symbols to type constructors.")

(defmacro define-type-constructor (name arity)
  `(setf (gethash ',name **type-definitions**)
         (make-tycon :name ',name :arity ',arity)))

(defun tycon-knownp (tycon-name)
  (check-type tycon-name symbol)
  (nth-value 1 (gethash tycon-name **type-definitions**)))

(defun find-tycon (tycon-name)
  (check-type tycon-name symbol)
  (or (gethash tycon-name **type-definitions**)
      (error "Couldn't find definition of tycon ~S" tycon-name)))

(defun (setf find-tycon) (new-value tycon-name)
  (check-type tycon-name symbol)
  (check-type new-value tycon)
  (when (tycon-knownp tycon-name)
    (style-warn "Clobbering tycon ~S" tycon-name))
  (setf (gethash tycon-name **type-definitions**) new-value))

(defun find-tycon-for-ctor (name)
  (loop :for tycon-name :being :the :hash-keys :of **type-definitions**
          :using (hash-value tycon)
        :when (find name (tycon-constructors tycon))
          :do (return tycon)
        :finally (return nil)))

;;; TY is forward declared in node.lisp

(defstruct (tyvar (:include ty)
                  (:constructor %make-tyvar))
  "A type variable."
  (id       0   :type integer          :read-only t)
  (instance nil :type (or null ty)     :read-only nil)
  (name     nil :type (or null symbol) :read-only nil))

(make-load-form-for-struct tyvar)

(defstruct (tyapp (:include ty)
                  (:constructor tyapp (constructor &rest types)))
  "A type application. (Note that this could be the application of a 0-arity constructor.)"
  (constructor  nil :type tycon     :read-only t)
  (types        nil :type type-list :read-only t))

(make-load-form-for-struct tyapp)

(defun tyapp-name (tyapp)
  (tycon-name (tyapp-constructor tyapp)))

;; We have a special constructor for functions because we handle
;; multi-argument functions without a separate tuple type.
(defstruct (tyfun (:include ty)
                  (:constructor tyfun (from to)))
  "A function type."
  (from nil :type type-list :read-only t)
  (to   nil :type ty        :read-only t))

(make-load-form-for-struct tyfun)

(defun tyfun-arity (tyfun)
  (length (tyfun-from tyfun)))

(defun function-type-p (ty)
  "Does the type TY represent a function type?"
  (check-type ty ty)
  (typep ty 'tyfun))

(defun type-arity (x)
  (cond
    ((function-type-p x) (tyfun-arity x))
    (t                   0)))

#+sbcl (declaim (sb-ext:freeze-type ty tyvar tyapp tyfun))

(defun more-or-equally-specific-type-p (general specific)
  "Is the type SPECIFIC an equal or more specific instantiation of GENERAL?"
  (check-type general ty)
  (check-type specific ty)
  (etypecase general
    ;; Anything could exist for SPECIFIC and it would be no more
    ;; general than GENERAL.
    (tyvar
     (or (null (tyvar-instance general))
         (more-or-equally-specific-type-p (tyvar-instance general) specific)))
    ;; TYAPPs and TYFUNs are only compatible with the same one.
    (tyapp
     (etypecase specific
       (tyvar (and (not (null (tyvar-instance specific)))
                   (more-or-equally-specific-type-p general (tyvar-instance specific))))
       (tyapp (and (eq (tyapp-constructor general)
                       (tyapp-constructor specific))
                   (every #'more-or-equally-specific-type-p
                          (tyapp-types general)
                          (tyapp-types specific))))
       (tyfun nil)))
    (tyfun
     (etypecase specific
       (tyvar (and (not (null (tyvar-instance specific)))
                   (more-or-equally-specific-type-p general (tyvar-instance specific))))
       (tyapp nil)
       (tyfun (and (more-or-equally-specific-type-p
                    (tyfun-to general) (tyfun-to specific))
                   (every #'more-or-equally-specific-type-p
                          (tyfun-from general)
                          (tyfun-from specific))))))))

(defvar *next-variable-id* 0)
(defun make-variable ()
  (prog1 (%make-tyvar :id *next-variable-id*)
    (incf *next-variable-id*)))

(defun variable-name (v)
  (or (tyvar-name v)
      (setf (tyvar-name v) (gensym "T"))))

(defun unparse-type (ty)
  (etypecase ty
    (tyvar
     (if (tyvar-instance ty)
         (unparse-type (tyvar-instance ty))
         (variable-name ty)))

    (tyapp
     (if (null (tyapp-types ty))
         (tyapp-name ty)
         (list* (tyapp-name ty) (mapcar #'unparse-type (tyapp-types ty)))))
    
    (tyfun
     (let ((from (mapcar #'unparse-type (tyfun-from ty)))
           (to (unparse-type (tyfun-to ty))))
       `(coalton:-> ,from ,to)))))

(defun prune (ty)
  (etypecase ty
    (tyvar
     (let ((instance (tyvar-instance ty)))
       (if (null instance)
           ty
           (setf (tyvar-instance ty) (prune instance)))))

    (tyapp
     ty)

    (tyfun
     ty)))

(defun occurs-in-type (v t2)
  (let ((pruned-t2 (prune t2)))
    ;; XXX is this RIGHT?
    (if (equalp v pruned-t2)
        t
        (typecase pruned-t2
          (tyapp (occurs-in v (tyapp-types pruned-t2)))
          (tyfun (occurs-in v (cons (tyfun-to pruned-t2) (tyfun-from pruned-t2))))
          (otherwise nil)))))

(defun occurs-in (ty types)
  (some (lambda (ty2) (occurs-in-type ty ty2)) types))

(defun is-generic (v non-generic)
  (not (occurs-in v non-generic)))

(defun fresh (ty &optional (non-generic nil))
  "Take a type, and substitute free variables with fresh ones."
  ;; XXX: Verify this hash table is correct.
  (let ((table (make-hash-table :test 'equalp)))
    (labels ((freshrec (tp)
               (let ((ptp (prune tp)))
                 (etypecase ptp
                   (tyvar
                    (if (not (is-generic ptp non-generic))
                        ptp
                        ;; XXX make correct
                        (multiple-value-bind (var exists?) (gethash ptp table)
                          (if exists?
                              var
                              (setf (gethash ptp table) (make-variable))))))
                   (tyapp
                    (apply #'tyapp
                           (tyapp-constructor ptp)
                           (mapcar #'freshrec (tyapp-types ptp))))
                   (tyfun
                    (tyfun (mapcar #'freshrec (tyfun-from ptp))
                           (freshrec (tyfun-to ptp))))))))
      (values (freshrec ty) (alexandria:hash-table-alist table)))))

(defun assoc-find (env name)
  (let ((entry (assoc name env)))
    (and entry
         (cdr entry))))


;;;;;;;;;;;;;;;;;;;;;;; Type Error Conditions ;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition coalton-type-error (error)
  ()
  (:documentation "A type error occuring in Coalton."))

(define-condition unification-error (coalton-type-error)
  ((first-type :initarg :first-type
               :reader unification-error-first-type)
   (second-type :initarg :second-type
                :reader unification-error-second-type))
  (:documentation "A general error indicating unification failure."))

(define-condition type-mismatch (unification-error)
  ((mismatched-types :initarg :mismatched-types
                     :reader type-mismatch-types))
  (:documentation "An error that is signalled when unification fails due to a type mismatch.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S are incompatible specifically ~
                          because ~S and ~S do not unify."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (first (type-mismatch-types c)))
                       (unparse-type (second (type-mismatch-types c))))))))

(define-condition arity-mismatch (type-mismatch)
  ((mismatched-arities :initarg :mismatched-arities
                       :reader arity-mismatch-arities))
  (:documentation "An error that is signalled when unification fails due to an arity mismatch.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S are incompatible specifically ~
                          because the function types ~S and ~S have different ~
                          arities (~D and ~D respectively)."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (first (type-mismatch-types c)))
                       (unparse-type (second (type-mismatch-types c)))
                       (first (arity-mismatch-arities c))
                       (second (arity-mismatch-arities c)))))))

(define-condition non-terminating-unification-error (unification-error)
  ((contained-type :initarg :contained-type
                   :reader non-terminating-unification-error-contained-type)
   (containing-type :initarg :containing-type
                    :reader non-terminating-unification-error-containing-type))
  (:documentation "An error that is signalled when unification would not terminate due to infinite recursion.")
  (:report (lambda (c s)
             (let ((*print-circle* nil)
                   (*print-pretty* nil))
               (format s "The types ~S and ~S cannot be unified because ~
                          it will cause infinite unification. Specifically,
                          ~S occurs in ~S."
                       (unparse-type (unification-error-first-type c))
                       (unparse-type (unification-error-second-type c))
                       (unparse-type (non-terminating-unification-error-contained-type c))
                       (unparse-type (non-terminating-unification-error-containing-type c)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Unification ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun unify (type1 type2)
  ;; We use LABELS just so we can have a copy of the original TYPE1
  ;; and TYPE2 for error reporting purposes.
  (labels ((%unify (ty1 ty2)
             (let ((pty1 (prune ty1))
                   (pty2 (prune ty2)))
               (cond
                 ((tyvar-p pty1)
                  (unless (equalp pty1 pty2)
                    (when (occurs-in-type pty1 pty2)
                      (error 'non-terminating-unification-error
                             :first-type type1
                             :second-type type2
                             :contained-type pty1
                             :containing-type pty2))
                    (setf (tyvar-instance pty1) pty2)))
                 ((tyvar-p pty2)
                  (%unify pty2 pty1))
                 ((and (tyfun-p pty1)
                       (tyfun-p pty2))
                  (let ((arity-1 (length (tyfun-from pty1)))
                        (arity-2 (length (tyfun-from pty2))))
                    (unless (= arity-1 arity-2)
                      (error 'arity-mismatch
                             :first-type type1
                             :second-type type2
                             :mismatched-types (list pty1 pty2)
                             :mismatched-arities (list arity-1 arity-2)))
                    (mapc #'%unify (tyfun-from pty1) (tyfun-from pty2))
                    (%unify (tyfun-to pty1) (tyfun-to pty2))))
                 ((and (tyapp-p pty1)
                       (tyapp-p pty2))
                  (let ((name1 (tyapp-name pty1)) (types1 (tyapp-types pty1))
                        (name2 (tyapp-name pty2)) (types2 (tyapp-types pty2)))
                    (when (or (not (eq name1 name2))
                              (not (= (length types1) (length types2))))
                      (error 'type-mismatch
                             :first-type type1
                             :second-type type2
                             :mismatched-types (list pty1 pty2)))
                    (mapc #'%unify types1 types2)))
                 (t
                  (error 'type-mismatch
                         :first-type type1
                         :second-type type2
                         :mismatched-types (list ty1 ty2)))))))
    (%unify type1 type2))
  nil)
