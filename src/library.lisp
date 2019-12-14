;;; library.lisp

(cl:in-package #:coalton-user)

(coalton-toplevel
  ;; Defined in early-types.lisp
  #+ignore
  (define-type Unit
    Singleton)

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

(cl:defmacro coalton:cond ((clause-a then-a) cl:&rest clauses)
  (cl:if (cl:not (cl:endp clauses))
         `(coalton:if ,clause-a
                      ,then-a
                      (coalton:cond ,@clauses))
         (cl:progn
           (cl:assert (cl:eq 'coalton:else clause-a) () "COND must have an ELSE clause.")
           then-a)))

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
  (define (ignore x)     Singleton)
  (define (identity x)   x)
  (define (constantly x) (fn (y) x))
  (define (flip f)       (fn (x y) (f y x)))
  (define (compose f g)  (fn (x) (f (g x))))
  (define (curry f)      (fn (x) (fn (y) (f x y)))))

;;; Boolean
(coalton-toplevel
  (define (not x) (if x False True))
  (define (complement f) (compose not f)))

;;; Strings
(coalton-toplevel
  (declare concat (-> (String String) String))
  (define (concat a b) (lisp String
                         (cl:concatenate 'cl:string a b)))

  (declare string-length (-> String Integer))
  (define (string-length s) (lisp Integer (cl:length s))))

;;; Comparators and Predicates
(cl:macrolet ((define-comparators (cl:&rest names)
                `(coalton-toplevel
                   ,@(cl:loop
                        :for op :in names
                        :for clop := (cl:intern (cl:symbol-name op) :cl)
                        :append (cl:list
                                 `(declare ,op (-> (Integer Integer) Boolean))
                                 `(define (,op x y) (lisp Boolean
                                                      (lisp-boolean-to-coalton-boolean
                                                       (,clop x y))))))))
              (define-predicates (cl:&rest names)
                `(coalton-toplevel
                   ,@(cl:loop
                        :for op :in names
                        :for clop := (cl:intern (cl:symbol-name op) :cl)
                        :append (cl:list
                                 `(declare ,op (-> (Integer) Boolean))
                                 `(define (,op x) (lisp Boolean
                                                    (lisp-boolean-to-coalton-boolean
                                                     (,clop x)))))))))
  (define-comparators = /= > < >= <=)
  (define-predicates evenp oddp plusp minusp zerop))

;;; Arithmetic
(coalton-toplevel
  (declare + (-> (Integer Integer) Integer))
  (define (+ x y) (lisp Integer (cl:+ x y)))
  (define (1+ x) (+ 1 x))

  (declare - (-> (Integer Integer) Integer))
  (define (- x y) (lisp Integer (cl:- x y)))
  (define (1- x) (- x 1))
  (define (~ x) (- 0 x))                ; ML-ism

  (declare * (-> (Integer Integer) Integer))
  (define (* x y) (lisp Integer (cl:* x y)))
  (define (double n) (* n 2))

  (declare / (-> (Integer Integer) Integer))
  (define (/ x y) (lisp Integer (cl:values (cl:floor x y))))
  (define (half n) (/ n 2))

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

(coalton-toplevel
  (define (gcd u v)
    (cond ((= u v)   u)
          ((zerop u) v)
          ((zerop v) u)
          ((evenp u) (if (evenp v)
                         (double (gcd (half u) (half v)))
                         (gcd (half u) v)))
          (else      (cond ((evenp v) (gcd u (half v)))
                           ((> u v)   (gcd (half (- u v)) v))
                           (else      (gcd (half (- v u)) u)))))))

;;; Random examples
(coalton-toplevel
  (define (fact1 n)
    (letrec ((%fact (fn (n answer)
                      (if (zerop n)
                          answer
                          (%fact (1- n) (* n answer))))))
      (%fact n 1)))

  (define (car x)
    (match x
      (Knil       (error "Can't take CAR of KNIL"))
      ((Kons a b) a)))

  (define (cdr x)
    (match x
      (Knil       Knil)
      ((Kons a b) b)))

  ;; (define (length l)  (fold (fn (x acc) (1+ acc)) 0 l))
  (define (length l)
    (letrec ((%length
              (fn (l n)
                (match l
                  (Knil       n)
                  ((Kons a b) (%length b (1+ n)))))))
      (%length l 0)))

  (define (map f x)
    (match x
      (Knil       Knil)
      ((Kons x xs) (Kons (f x) (map f xs)))))

  (define (mapper f) (fn x (map f x)))

  (define (fold f init l)
    (match l
      (Knil        init)
      ((Kons x xs) (fold f (f x init) xs))))

  (define (tabulate f n)
    (letrec ((%tabulate
              (fn (n l)
                (if (zerop n)
                    l
                    (%tabulate (1- n)
                               (Kons (f (1- n)) l))))))
      (%tabulate n Knil))))

(coalton-toplevel
  (define (reverse l) (fold Kons Knil l))
  (define (sum l)     (fold + 0 l))
  (define (product l) (fold * 1 l))

  (define (keep-if f l)                 ; AKA filter
    (fold (fn (x acc)
            (if (f x)
                (Kons x acc)
                acc))
          Knil
          l))

  (define (remove-if f l) (keep-if (complement f) l))

  (define (replicate x n) (tabulate (constantly x) n))
  (define (iota n) (tabulate identity n))
  (define (range a b) (tabulate ((curry +) a) (- b a)))

  (define (fact2 n) (product (range 1 (1+ n)))))

;; Grab Bag
(coalton-toplevel
  (declare integer-name (-> Integer String))
  (define (integer-name n)
    (lisp String
      (cl:format cl:nil "~R" n))))
