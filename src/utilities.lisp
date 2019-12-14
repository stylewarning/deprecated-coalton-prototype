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

(defun make-collector ()
  (let* ((tail  (cons nil nil))
         (items tail))
    (lambda (&optional (item nil itemp))
      (cond
        ((not itemp) (cdr items))
        (t
         (rplacd tail (cons item nil))
         (setf   tail (cdr tail))
         item)))))

(defmacro record-side-effects! ((collector) &body forms)
  `(progn
     ,@forms
     (funcall ,collector '(progn ,@forms))))

(defmacro make-load-form-for-struct (struct-name)
  `(defmethod make-load-form ((s ,struct-name) &optional env)
     (make-load-form-saving-slots s :environment env)))

(defun style-warn (format-control &rest format-args)
  (format t "~&; ")
  (apply #'format t format-control format-args)
  (fresh-line))
