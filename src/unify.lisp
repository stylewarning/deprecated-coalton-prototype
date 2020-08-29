;;;; unify.lisp
;;;;
;;;; Copyright (c) 2020 Robert Smith

(in-package #:coalton-impl)

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

