;;; library.lisp

(in-package #:coalton-user)

(coalton
  (define-type Void)

  #+ignore
  (define-type Unit
    Singleton)

  #+ignore
  (define-type Boolean
    True
    False)

  (define-type (Maybe t)
    Nothing
    (Just t))

  (define-type (Either a b)
    (Left a)
    (Right b))

  (define-type (Liszt t)
    Knil
    (Kons t (Liszt t))))

(coalton
  (define (identity x) x))

(coalton                                ; For testing
  (declare 1+ (-> Integer Integer))
  (declare ignore (-> a Unit))
  (declare not (-> Boolean Boolean)))
