;;;; impl-debug-routines.lisp

(in-package #:coalton-impl)

;;; This file has some general debug and introspection routines that
;;; are useful for development, but are not needed or a part of the
;;; functionality of the system.

(defun print-value-database (&key (symbol-table **global-value-definitions**)
                                  (stream *standard-output*))
  ;; Print package prefixes.
  (let ((*package* (find-package "KEYWORD"))
        (*print-pretty* nil))
    (maphash (lambda (sym entry)
               (format stream "~A (package: ~A) (internal: ~S)~%    :: ~A~%"
                       sym
                       (package-name (symbol-package sym))
                       (entry-internal-name entry)
                       (unparse-type (or (entry-declared-type entry)
                                         (entry-derived-type entry)))))
             symbol-table))
  (values))
