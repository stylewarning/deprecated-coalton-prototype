;;;; example.lisp
;;;;
;;;; Some example Coalton code?

(in-package #:coalton)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Works ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Identity.
(coalton
  (declare identity (-> t t))
  (define (identity x) x))

;;; Simple type definitions.
(coalton
  (define-type Void)

  (define-type Unit
    Singleton)

  (define-type Bool
    True
    False)

  (define-type (Maybe t)
    Nothing
    (Just t))

  (define-type (Liszt t)
    Knil
    (Kons t (Liszt t)))

  (define-type (Either a b)
    (Left a)
    (Right b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Does Not Work ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simple pattern matching.
(coalton
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
