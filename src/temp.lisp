(in-package :coalton-impl)

(defparameter *eq-class
  (make-type-class
   :name 'eq
   :members (list
             (cons '== (parse-type-expression '(coalton:fn a a coalton:-> coalton:boolean))))))

(defparameter *countable-class
  (make-type-class
   :name 'countable
   :members (list
             (cons 'size (parse-type-expression '(coalton:fn a coalton:-> coalton:integer))))))

(defparameter *eq-int
  (make-class-instance
   :type-class 'eq
   :instantiated-type (make-cty (parse-type-expression 'coalton:integer))
   :implementations (vector
                     (cons '== (parse-form 'coalton-user::=)))))

(unless (var-knownp '==)
  (forward-declare-variable '== (parse-type-expression
                                 '(coalton:for (Eq a) coalton:=> (coalton:fn a a coalton:-> coalton:Boolean)))
                            t))

(unless (var-knownp 'gt)
  (forward-declare-variable 'gt (parse-type-expression
                                 '(coalton:for (Ord a) coalton:=> (coalton:fn a a coalton:-> coalton:Boolean)))
                            t))
