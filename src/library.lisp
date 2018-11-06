;;; library.lisp

(cl:in-package #:coalton-user)

(coalton-toplevel
  ;; Defined in early-types.lisp
  #+ignore
  (define-type Unit
    Singleton)

  ;; Defined in early-types.lisp
  #+ignore
  (define-type Boolean
    True
    False)

  (define-type Void)

  (define-type (Maybe t)
    Nothing
    (Just t))

  (define-type (Either a b)
    (Left a)
    (Right b))

  (define-type (Liszt t)
    Knil
    (Kons t (Liszt t))))

;;; Combinators
(coalton-toplevel
  (define (ignore x) Singleton)
  (define (identity x) x)
  (define (constantly x) (fn (y) x))
  (define (flip f) (fn (x y) (f y x))))

;;; Boolean
(coalton-toplevel
  (define (not x) (if x False True)))

;;; Arithmetic
(coalton-toplevel
  (declare = (-> (Integer Integer) Boolean))
  (define (= x y) (lisp Boolean (coalton-impl::lisp-boolean-to-coalton-boolean
                                 (cl:= x y))))

  (define (zerop x) (= x 0))

  (declare + (-> (Integer Integer) Integer))
  (define (+ x y) (lisp Integer (cl:+ x y)))
  (define (1+ x) (+ 1 x))

  (declare - (-> (Integer Integer) Integer))
  (define (- x y) (lisp Integer (cl:- x y)))
  (define (1- x) (- x 1))
  (define (~ x) (- 0 x))                ; ML-ism

  (declare * (-> (Integer Integer) Integer))
  (define (* x y) (lisp Integer (cl:* x y)))

  (declare / (-> (Integer Integer) Integer))
  (define (/ x y) (lisp Integer (cl:values (cl:floor x y))))
  (define (safe-/ x y) (if (zerop y)
                           Nothing
                           (Just (/ x y)))))

;;; Random examples
(coalton-toplevel
  (define (fact n)
    (letrec ((fact-aux (fn (n answer)
                         (if (zerop n)
                             answer
                             (fact-aux (1- n) (* n answer))))))
      (fact-aux n 1)))

  (declare Knil? (-> ((Liszt t)) Boolean))
  (define (Knil? x)
    (lisp Boolean
      (coalton-impl::lisp-boolean-to-coalton-boolean
       (cl:typep x 'Knil))))

  (declare Kons? (-> ((Liszt t)) Boolean))
  (define (Kons? x)
    (lisp Boolean
      (coalton-impl::lisp-boolean-to-coalton-boolean
       (cl:typep x 'Kons))))

  (declare car (-> ((Liszt t)) t))
  (define (car x)
    (if (Knil? x)                       ; match CL behavior
        Knil
        (lisp t
          (cl:svref (cl:slot-value x 'coalton-impl::value) 0))))

  (declare cdr (-> ((Liszt t)) (Liszt t)))
  (define (cdr x)
    (if (Knil? x)                       ; match CL behavior
        Knil
        (lisp t
          (cl:svref (cl:slot-value x 'coalton-impl::value) 1))))

  (define (length l)
    (letrec ((len (fn (l n)
                    (if (Knil? l)
                        n
                        (len (cdr l) (1+ n))))))
      (len l 0)))

  (define (map f x)
    (if (Knil? x)
        Knil
        (Kons (f (car x)) (map f (cdr x)))))

  (define (mapper f) (fn x (map f x))))
