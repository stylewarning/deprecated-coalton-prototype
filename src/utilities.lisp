;;;; utilities.lisp

(in-package #:coalton-impl)

(defun required (name)
  (error "The argument/slot ~S is required." name))

(declaim (inline boolify))
(defun boolify (thing)
  (if thing t nil))

(define-condition coalton-parse-error (error)
  ((form :initarg :form
         :reader coalton-parse-error-form)
   (reason-control :initarg :reason-control
                   :reader coalton-parse-error-reason-control)
   (reason-args :initarg :reason-args
                :reader coalton-parse-error-reason-args))
  (:report (lambda (c s)
             (format s "Failed to parse ~S because: ~?"
                     (coalton-parse-error-form c)
                     (coalton-parse-error-reason-control c)
                     (coalton-parse-error-reason-args c)))))

(defun error-parsing (form reason-control &rest reason-args)
  (error 'coalton-parse-error
         :form form
         :reason-control reason-control
         :reason-args reason-args))

(defun style-warn (format-control &rest format-args)
  (apply #'alexandria:simple-style-warning format-control format-args))

(defmacro define-symbol-property (property-accessor &key
                                                      (type nil type-provided)
                                                      (documentation nil doc-provided))
  ;; TODO document
  ;;
  ;; TODO use docs and type
  (declare (ignore type type-provided documentation doc-provided))
  (let ((symbol (gensym "SYMBOL"))
        (new-value (gensym "NEW-VALUE")))
    `(progn
       (declaim (inline ,property-accessor (setf ,property-accessor)))
       (defun ,property-accessor (,symbol)
         (get ,symbol ',property-accessor))
       (defun (setf ,property-accessor) (,new-value ,symbol)
         (setf (get ,symbol ',property-accessor) ,new-value)))))
