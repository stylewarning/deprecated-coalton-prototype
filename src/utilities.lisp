;;;; utilities.lisp

(in-package #:coalton-impl)

(defun required (name)
  (error "The argument/slot ~S is required." name))

(declaim (inline boolify))
(defun boolify (thing)
  (if thing t nil))

(define-condition coalton-parse-error (error)
  ((form :initarg :form
         :accessor coalton-parse-error-form)
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

(defmacro with-parsing-context ((form) &body body)
  `(handler-case
       (progn ,@body)
     (coalton-parse-error (err)
       (setf (coalton-parse-error-form err) ,form)
       (error err))))

(defun style-warn (format-control &rest format-args)
  (apply #'alexandria:simple-style-warning format-control format-args))

(defmacro include-if (condition &body body)
  `(when ,condition
     (list ,@ (remove nil body))))

(defmacro define-symbol-property (property-accessor &key
                                                      (type nil type-provided)
                                                      documentation)
  "Define an accessor for a symbol property.

Implementation notes: These notes aren't relevant to users of this macro, but are Good To Know.

    * The symbol's property is stored as a part of the symbol's plist.

    * The plist key is just the name of the accessor.
    "
  (check-type property-accessor symbol)
  (check-type documentation (or null string))
  (let ((symbol (gensym "SYMBOL"))
        (new-value (gensym "NEW-VALUE")))
    `(progn
       (declaim (inline ,property-accessor (setf ,property-accessor)))
       ,@(include-if type-provided
           `(declaim (ftype (function (symbol) (or null ,type)) ,property-accessor))
           `(declaim (ftype (function (,type symbol) ,type) (setf ,property-accessor))))
       (defun ,property-accessor (,symbol)
         ,@(include-if documentation documentation)
         (get ,symbol ',property-accessor))
       (defun (setf ,property-accessor) (,new-value ,symbol)
         (setf (get ,symbol ',property-accessor) ,new-value))
       ;; Return the name defined.
       ',property-accessor)))
