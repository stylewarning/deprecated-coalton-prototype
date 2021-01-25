;;;; class.lisp

(in-package #:coalton-impl)

(defstruct type-class
  (name (required 'name) :type symbol :read-only t)
  (variables (required 'variables) :type alexandria:proper-list :read-only t)

  ;; TODO: This currently is METHOD-NAME -> TYPE. Should this be different?
  ;; Hash table from METHOD-NAME to TYPE-CLASS-METHOD
  (methods (make-hash-table) :type hash-table :read-only t)

  ;; Hash table from TYPES to TYPE-CLASS-INSTANCE
  (instances (make-hash-table :test #'equalp) :type hash-table :read-only t))

(defstruct type-class-method
  (name (required 'name) :type symbol :read-only t)
  (type (required 'type) :type ty :read-only t))

(define-global-var **class-definitions** (make-hash-table :test 'eql)
  "Database of Coalton class definitions.")

;; METHOD-NAME -> CLASS
(define-global-var **method-definitions** (make-hash-table :test 'eql)
  "Database of Coalton method definitions.")

(defun class-knownp (class-name)
  (check-type class-name symbol)
  (nth-value 1 (gethash class-name **class-definitions**)))

(defun find-type-class (class-name)
  (check-type class-name symbol)
  (nth-value 0 (gethash class-name **class-definitions**)))

(defun class-instance-knownp (class-name types)
  (check-type class-name symbol)
  (check-type types type-list)
  (multiple-value-bind (class class-found?)
      (gethash class-name **class-definitions**)
    (and class-found?
         (nth-value 1 (gethash types (type-class-instances class))))))

(defun find-class-instance (class-name types)
  (check-type class-name symbol)
  (check-type types type-list)
  (multiple-value-bind (class class-found?)
      (gethash class-name **class-definitions**)
    (and class-found?
         (nth-value 0 (gethash types (type-class-instances class))))))

(defstruct type-class-instance
  ;; Do we need constraints? maybe yes maybe no. Maybe they go here
  (types (required 'types) :type alexandria:proper-list :read-only t)
  ;; What is an implementation? method maybe?

  ;; hash table from method name to TYPE-CLASS-INSTANCE-METHOD
  (methods (required 'methods) :type hash-table :read-only t))

(defstruct type-class-instance-method
  (name (required 'name) :type symbol :read-only t)
  (type (required 'type) :type ty :read-only t)
  (implementation (required 'implementation) :type symbol :read-only t))

;;; Symbol generating functions
(defun generate-instance-method-symbol (class-name type-name method-name)
  ;; TODO: These should get another package... maybe coalton-internal?
  ;;       This could be a good opportunity to consolidate all the state
  ;;       so that it can be saved/loaded with a fasl.
  (intern (format nil "~S/~S/~S" class-name type-name method-name)
          ':coalton-impl))
