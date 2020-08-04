;;;; types.lisp

(in-package #:coalton-impl)

;;; This file mostly concerns the representation of types.
;;;
;;; unify.lisp also has some useful utilities acting on types.

(defstruct (cx (:constructor cx (class type)))
  "A constraint (abbreviated CX). This is an object of the set Q in the constrained type

    Q => T.

For instance, in (Eq t, Ord t) => t -> t, there are two CX's, \"Eq t\" and \"Ord t\"."
  (class nil :type symbol :read-only t)
  (type  nil :type ty     :read-only t))

;;; A type constructor is not a constructor for a value, but a
;;; constructor for a *type*! Get with the program!
(defstruct tycon
  "A constructor for type applications."
  ;; The name of the TYCON. Technically this is only used for printing!
  ;;
  ;; We also incidentally rely on this in COMPILE-VALUE so that we can
  ;; construct a constructor's class name.
  (name (required 'name) :type symbol                 :read-only t)
  ;; Was this tycon redefined in the global database, so the above
  ;; name no longer makes sense?
  (invalidated nil       :type boolean)
  ;; The number of (type) arguments the tycon can take.
  (arity 0               :type unsigned-byte          :read-only t)
  ;; A list of CONSTRUCTOR-NAMEs.
  ;;
  ;; The CONSTRUCTOR-NAME names a known function that constructs a
  ;; value of TYCON type.
  ;;
  ;; This isn't read-only because we might set it later. It would be
  ;; nice to make it read-only though.
  (constructors nil      :type alexandria:proper-list))

(defstruct (cty (:constructor make-cty (expr &key constraints)))
  "A type with constraints on quantified variables."
  (constraints nil :type list :read-only t) ; List of CX
  (expr        nil :type ty   :read-only t))

;;; TY is forward declared in node.lisp

(defstruct (tyvar (:include ty)
                  (:constructor %make-tyvar))
  "A type variable."
  (id       0   :type integer          :read-only t)
  (instance nil :type (or null ty)     :read-only nil)
  (name     nil :type (or null symbol) :read-only nil))

(defstruct (tyapp (:include ty)
                  (:constructor tyapp (constructor &rest types)))
  "A type application. (Note that this could be the application of a 0-arity constructor.)"
  (constructor  nil :type tycon     :read-only t)
  (types        nil :type type-list :read-only t))

(defun tyapp-name (tyapp)
  (let ((tycon (tyapp-constructor tyapp)))
    (if (tycon-invalidated tycon)
        ;; A little hack so that we see when tycons got invalidated.
        '#:@@INVALIDATED@@
        (tycon-name tycon))))

;; We have a special constructor for functions because we handle
;; multi-argument functions without a separate tuple type.
(defstruct (tyfun (:include ty)
                  (:constructor tyfun (from to)))
  "A function type."
  (from nil :type type-list :read-only t)
  (to   nil :type ty        :read-only t))

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

(defun type= (type1 type2)
  "Check equality of types TYPE1 and TYPE2

Types are equivalent when the structure (TYAPP and TYFUN) matches and there exists a bijection between TYVARs of each type."
  (declare (type ty type1 type2)
           (values boolean))
  ;; VAR-TABLE is an alist with entries
  ;;
  ;;     (tyvar-id . tyvar-id)
  (let ((var-table nil))
    (labels ((%type= (ty1 ty2)
               (let ((pty1 (prune ty1))
                     (pty2 (prune ty2)))
                 (cond
                   ((and (tyvar-p pty1)
                         (tyvar-p pty2))
                    (let ((pair1 (find (tyvar-id pty1) var-table :key #'car))
                          (pair2 (find (tyvar-id pty2) var-table :key #'car)))
                      (cond
                        ((and (null pair1) (null pair2))
                         ;; Push both (ID1 ID2) and (ID2 ID1) onto the
                         ;; table.
                         (pushnew (cons (tyvar-id pty1) (tyvar-id pty2)) var-table :key #'car)
                         (pushnew (cons (tyvar-id pty2) (tyvar-id pty1)) var-table :key #'car)
                         ;; Assume these types are equal.
                         t)
                        ((or (null pair1) (null pair2))
                         ;; If a match was found for one and not the
                         ;; other, it's guaranteed not equal.
                         nil)
                        (t
                         ;; Check that A -> B and B -> A are compatible.
                         (and (eql (car pair1) (cdr pair2))
                              (eql (cdr pair1) (car pair2)))))))
                   ((and (tyfun-p pty1)
                         (tyfun-p pty2))
                    (and
                     (= (tyfun-arity pty1) (tyfun-arity pty2))
                     (%type= (tyfun-to pty1) (tyfun-to pty2))
                     (every #'%type= (tyfun-from pty1) (tyfun-from pty2))))
                   ((and (tyapp-p pty1)
                         (tyapp-p pty2))
                    (let ((name1 (tyapp-name pty1)) (types1 (tyapp-types pty1))
                          (name2 (tyapp-name pty2)) (types2 (tyapp-types pty2)))
                      (and (eq name1 name2)
                           (= (length types1) (length types2))
                           (every #'%type= types1 types2))))
                   (t
                    nil)))))
      (%type= type1 type2))))

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
