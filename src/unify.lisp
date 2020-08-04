;;;; unify.lisp

(in-package #:coalton-impl)

;;; This file contains the unification algorithm, along with a few
;;; utilities (namely, utilities to make union-find possible).

(defvar *next-variable-id* 0)
(defun make-variable ()
  (%make-tyvar (incf *next-variable-id*)))

(defun variable-name (v)
  (or (tyvar-name v)
      (setf (tyvar-name v) (gensym "T"))))

(defun extract-variables (ty)
  "Extract variables from the type expression TY."
  (let ((vars '()))
    (labels ((descend (ty)
               (etypecase ty
                 (tyvar
                  (push ty vars))

                 (tyapp
                  (mapc #'descend (tyapp-types ty)))

                 (tyfun
                  (mapc #'descend (tyfun-from ty))
                  (descend (tyfun-to ty))))))
      (descend ty))
    vars))

(defun unparse-type (ty)
  "Convert a type TY back into an S-expression representation (which could be parsed back again with PARSE-TYPE)."
  (etypecase ty
    ;; Constrained types
    (cty
     `(coalton:for ,@(mapcar (lambda (cx)
                               `(,(cx-class cx) ,(unparse-type (cx-type cx))))
                             (cty-constraints ty))
                   coalton:=>
                   ,(unparse-type (cty-expr ty))))

    ;; TY substructures
    (tyvar
     (if (tyvar-instance ty)
         (unparse-type (tyvar-instance ty))
         (variable-name ty)))

    (tyapp
     (if (null (tyapp-types ty))
         (tyapp-name ty)
         (cons (tyapp-name ty) (mapcar #'unparse-type (tyapp-types ty)))))

    (tyfun
     (let ((from (mapcar #'unparse-type (tyfun-from ty)))
           (to (unparse-type (tyfun-to ty))))
       `(coalton:fn ,@from coalton:-> ,to)))))

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
     ty)

    (cty
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
                   (cty
                    (make-cty (freshrec (cty-expr ptp))
                              :constraints (mapcar (lambda (cx)
                                                     (cx (cx-class cx)
                                                         (freshrec (cx-type cx))))
                                                   (cty-constraints ptp))))
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
