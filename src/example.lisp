;;;; example.lisp
;;;;
;;;; Some example Coalton code?

(in-package #:coalton)

;;; Identity.
(coalton
  (declare identity (-> t t))
  (define (identity x) x))

;;; Simple type definitions.
(coalton
  (define-type Unit)
  
  (define-type Boolean
    True
    False))

;;; The quintessential list.
(coalton
  (define-type (Liszt t)
    Knil
    (Kons t List))
  
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

;;; Weird order of mutually recursive functions.
(coalton
  (define f (+ 1 (h 5)))
  (define g (if (h true) 1 0))
  (define (h x) x))

;;; Mutually recursive types.
(coalton
  (define-type Red
    (RedNil)
    (RedCons Black Red))
  (define-type Black
    (BlackNil)
    (BlackCons Red Black)))
