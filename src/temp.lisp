(in-package :coalton-impl)

(defparameter *eq-class
  (make-type-class
   :name 'Eq
   :members (list
             (cons '== (parse-type-expression '(coalton:fn a a coalton:-> coalton:boolean))))))

(defparameter *countable-class
  (make-type-class
   :name 'Countable
   :members (list
             (cons 'size (parse-type-expression '(coalton:fn a coalton:-> coalton:integer))))))

(defparameter *eq-int
  (make-class-instance
   :type-class 'Eq
   :instantiated-type (cty integer-type)
   :implementations (vector
                     (cons '== (parse-form 'coalton-user::=)))))

(unless (var-knownp '==)
  (forward-declare-variable '== (parse-type-expression
                                 '(coalton:for (Eq a) coalton:=> (coalton:fn a a coalton:-> coalton:Boolean)))
                            t))

(unless (var-knownp 'size)
  (forward-declare-variable 'size (parse-type-expression
                                   '(coalton:for (Countable a) coalton:=> (coalton:fn a coalton:-> coalton:Integer)))
                            t))

(defparameter *expr*
  '(coalton:fn (n x)
    (== n (size x))))
