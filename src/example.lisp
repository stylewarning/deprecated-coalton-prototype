;;;; example.lisp
;;;;
;;;; Some example Coalton code?

(in-package #:coalton-user)

(coalton-toplevel
  ;; Mutual type recursion (example 1)
  (define-type Chess-Piece
    King
    Queen
    Rook
    Bishop
    Knight
    Pawn)

  (define-type White-Move
    (White-Move-To Chess-Piece Integer Integer Black-Move)
    Black-Checkmate)

  (define-type Black-Move
    (Black-Move-To Chess-Piece Integer Integer White-Move)
    White-Checkmate)
  
  ;; Mutual type recursion (example 2)
  (define-type Red
    (Red-Nil)
    (Red-Cons Black Red))
  
  (define-type Black
    (Black-Nil)
    (Black-Cons Red Black))
  
  ;; Funny order to some functions (the type of H should be T -> T)
  (define f (+ 1 (h 5)))
  (define g (if (h true) 1 0))
  (define (h x) x)
  
  ;; Plain old mutually recursive functions
  (define (evenp x)
    (if (zerop x)
        True
        (oddp (- x 1))))
  (define (oddp x)
    (if (zerop x)
        False
        (evenp (- x 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Does Not Work ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Simple pattern matching.
#+ignore
(coalton
  (declare length (forall t (-> (List t) Integer)))
  (define (length x)
    (match x
      (Nil 0)
      ((Cons x xs) (+ 1 (length xs))))))


