;;; library.lisp

(cl:in-package #:coalton-user)

(coalton-toplevel
  ;; Defined in early-types.lisp
  #+ignore
  (define-type Unit
    Singleton)

  ;; Defined in early-types.lisp
  (define-type coalton:Boolean
    coalton:True
    coalton:False)

  ;; Defined in early-types.lisp
  #+ignore
  (define-type Void)

  (define-type (Maybe t)
    Nothing
    (Just t))

  (define-type (Either a b)
    (Left a)
    (Right b))

  (define-type (Liszt t)
    Knil
    (Kons t (Liszt t)))

  (define-type (Binary-Tree s t)
    (Leaf s)
    (Branch t (Binary-Tree s t) (Binary-Tree s t))))

(cl:defmacro coalton:if (expr then else)
  `(match ,expr
     (coalton:true ,then)
     (coalton:false ,else)))

(cl:declaim (cl:inline lisp-boolean-to-coalton-boolean))
(cl:defun lisp-boolean-to-coalton-boolean (x)
  (cl:if x coalton:true coalton:false))



;;; Erroring
(coalton-toplevel
 (declare error (-> String t))
 (define (error str)
   (lisp t (cl:error "~A" str))))

;;; Combinators
(coalton-toplevel
  (define (ignore x) Singleton)
  (define (identity x) x)
  (define (constantly x) (fn (y) x))
  (define (flip f) (fn (x y) (f y x)))
  (define (compose f g) (fn (x) (f (g x)))))

;;; Boolean
(coalton-toplevel
  (define (not x) (if x False True)))

;;; Strings
(coalton-toplevel
  (declare concat (-> (String String) String))
  (define (concat a b) (lisp String
                         (cl:concatenate 'cl:string a b)))

  (declare string-length (-> String Integer))
  (define (string-length s) (lisp Integer (cl:length s))))

;;; Arithmetic
(coalton-toplevel
  (declare = (-> (Integer Integer) Boolean))
  (define (= x y) (lisp Boolean
                    (lisp-boolean-to-coalton-boolean
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

;;; Mutable Cells
(coalton-toplevel
  (define-type (Mutable-Cell t)
    (Ref t))

  (declare mutate-cell (-> ((Mutable-Cell t) t) Unit))
  (define (mutate-cell r v)
    (lisp Unit
      (cl:progn
        (cl:setf (cl:svref (cl:slot-value r 'coalton-impl::value) 0) v)
        Singleton))))

;;; Random examples
(coalton-toplevel
  (define (fact n)
    (letrec ((fact-aux (fn (n answer)
                         (if (zerop n)
                             answer
                             (fact-aux (1- n) (* n answer))))))
      (fact-aux n 1)))

  (define (car x)
    (match x
      ((Kons a b) a)
      (Knil       (error "Can't take CAR of KNIL"))))

  (define (cdr x)
    (match x
      ((Kons a b) b)
      (Knil       Knil)))

  (define (length l)
    (letrec ((len (fn (l n)
                    (match l
                      ((Kons a b)
                       (len b (1+ n)))
                      (Knil
                       n)))))
      (len l 0)))

  (define (map f x)
    (match x
      ((Kons a b) (Kons (f a) (map f b)))
      (Knil       Knil)))

  (define (mapper f) (fn x (map f x))))

;; Grab Bag
(coalton-toplevel
  (declare integer-name (-> Integer String))
  (define (integer-name n)
    (lisp String
      (cl:format cl:nil "~R" n))))
