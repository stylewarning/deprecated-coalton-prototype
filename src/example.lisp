;;;; example.lisp
;;;;
;;;; Some example Coalton code?

(in-package #:coalton)

(coalton
  (declare identity (forall t (-> t t)))
  (define (identity x) x)

  (define-type (List t)
    (nil)
    (cons t List))
  
  (declare length (forall t (-> (List t) Integer)))
  (define (length x)
    (match x
      (nil 0)
      ((cons x xs) (+ 1 (length xs))))))
