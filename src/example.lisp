;;;; example.lisp
;;;;
;;;; Some example Coalton code?

(in-package #:coalton)

;;; Identity.
(coalton
  (declare identity (forall t (-> t t)))
  (define (identity x) x))

;;; Simple type definitions.
(coalton
  (define-type Unit)
  
  (define-type Boolean
    True
    False))

;;; The quintessential list.
(coalton
  (define-type (List t)
    (Nil)
    (Cons t List))
  
  (declare length (forall t (-> (List t) Integer)))
  (define (length x)
    (match x
      (Nil 0)
      ((Cons x xs) (+ 1 (length xs))))))

;;; Mutually recursive functions.
(coalton
  (define (evenp x)
    (if (zerop x)
        True
        (oddp (- x 1))))
  (define (oddp x)
    (if (zerop x)
        False
        (evenp (- x 1)))))

;;; Mutually recursive types.
(coalton
  (define-type Red
    (RedNil)
    (RedCons Black Red))
  (define-type Black
    (BlackNil)
    (BlackCons Red Black)))
