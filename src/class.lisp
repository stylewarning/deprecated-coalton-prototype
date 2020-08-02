;;;; class.lisp

(in-package #:coalton-impl)

(defstruct type-class
  (name (required 'name) :type symbol :read-only t)

  ;; List of conses (A . B) where A is a symbol of tha function name
  ;; and B is the ty.
  (members nil           :type alexandria:proper-list :read-only t))

(define-global-var **class-definitions** (make-hash-table :test 'eql)
  "Database of Coalton type class definitions. These are mappings from symbols to ...")

(defun find-type-class (name)
  (nth-value 0 (gethash name **class-definitions**)))


(defstruct class-instance
  (type-class (required 'type-class) :type symbol :read-only t)
  (instantiated-type (required 'instantiated-type) :type cty :read-only t)
  (implementations (required 'implementations) :type vector :read-only t))

