;;;; coalton.asd

(asdf:defsystem #:coalton
  :description "A dialect of ML in Common Lisp."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on (#:alexandria
               #:global-vars
               #:trivial-garbage
               #:optima
               #:abstract-classes
               #:singleton-classes)
  :in-order-to ((asdf:test-op (asdf:test-op #:coalton/tests)))
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "utilities")
               (:file "global-lexical")
               (:file "node")
               (:file "types")
               (:file "parse-type")
               (:file "parse-value")
               (:file "global-environment")
               (:file "early-types")
               (:file "hindley-milner")
               (:file "free-variables")
               (:file "sort-letrec")
               (:file "compile-value")
               (:file "toplevel-declare")
               (:file "toplevel-define-type")
               (:file "toplevel-define")
               (:file "coalton")
               (:file "faux-macros")
               (:file "impl-debug-routines")
               (:file "library")))

(asdf:defsystem #:coalton/tests
  :description "Tests for COALTON."
  :author "Robert Smith <robert@stylewarning.com>"
  :license "MIT"
  :depends-on (#:coalton #:fiasco)
  :perform (asdf:test-op (o s) nil)
  :pathname "tests/"
  :serial t
  :components ((:file "package")))
