;;;; sort-letrec.lisp

(in-package #:coalton-impl)

;;; The purpose of this file is to provide "letrec sorting". This is
;;; useful for both the toplevel as well as COALTON:LETREC.
;;;
;;; Topological sort code taken from:
;;;
;;;     https://github.com/tarballs-are-good/lisp-random/blob/master/tsort.lisp
;;;
;;; which is copyright (c) 2012 Robert Smith.

;;; A directed acyclic graph should be specified as follows:
;;;
;;;    ((<node> <dependency> ...)
;;;     ...
;;;    )

(defun list-sinks (dag)
  "Find the sinks in DAG."
  (loop :for (node . deps) :in dag
        :when (null deps)
          :collect node))

(defun clean-up-self-referential (dag)
  (loop :for thing :in dag
        :for (node . deps) := thing
        :when (member node deps)
          :collect (progn
                     (setf (cdr thing) (delete node deps))
                     node)
            :into self-referential
        :finally (return (values dag self-referential))))

(defun tsort (input-dag)
  "Find the topological sort of GRAPH destructively. Return two values:

1. The best topological sort that could be done (a list of <node>s)

2. The rest of the unsortable DAG that _only_ contains cycles.

3. The nodes that were self-referential.
"
  (multiple-value-bind (dag self-referential)
      (clean-up-self-referential input-dag)
    (let* ((sorted nil)                   ; Final sorted list.
           (sinks  (list-sinks dag)))     ; Sinks in the graph.
      (loop :while (not (null sinks))
            :do (progn
                  ;; Remove the sinks.
                  (setf dag (delete-if (lambda (x) (null (cdr x))) dag))

                  ;; Get the next sink.
                  (let ((sink (pop sinks)))

                    ;; Add it to the sorted list.
                    (push sink sorted)

                    ;; For every node/neighborhood...
                    (dolist (node dag)
                      ;; Remove the sink from the dependencies if any
                      ;; exist.
                      (setf (cdr node) (delete sink (cdr node)))

                      ;; If we have no more dependencies, add it to the
                      ;; sinks.
                      (when (null (cdr node))
                        (push (car node) sinks)))))
            :finally (return (values (nreverse sorted) dag self-referential))))))

(defun letrec-bindings-to-dag (vars vals)
  ;; VARs are the variables being bound
  ;;
  ;; VALs are the NODEs being bound to
  (loop :for var :in vars
        :for val :in vals
        :collect (cons var (copy-list (intersection vars (free-variables val))))))

(defun sort-letrec-bindings (vars vals)
  (tsort (letrec-bindings-to-dag vars vals)))
