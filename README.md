# Coalton

Coalton is a dialect of ML embedded in Common Lisp. It emphasizes practicality and interoperability with Lisp, and is intended to be a DSL that allows one to gradually make their programs safer.

Coalton currently allows one to do the following in Common Lisp:

* Express statically typed programs similar in spirit to Standard ML, OCaml, and Haskell.
* Perform compile-time type inference and type checking à la Hindley-Milner type inference.
  * Global values are inferred by default, but can be manually declared if desired.
* Interoperate an ML-like with Common Lisp, in both directions. All data values are native and easy data structures.
* Define parameterized algebraic data types, including mutually recursive types.

Coalton has some limitations:

* [bug] Unit tests aren't at all written. USE AT YOUR OWN RISK!
* [bug] Redefinitions don't play well in a lot of ways:
  * Types aren't globally re-checked or warned about.
  * The internal type DB is frequently clobbered with new data.
* [bug] Declared types *are* used for inference, but are simply believed without question. As such, a bug in a declaration will cause *runtime* bugs.
* [bug] Pattern matching is not yet implemented.
* Generalized algebraic data types, type classes, and high-order types are neither implemented nor supported.
* ML structures, signatures, or functors are neither implemented nor supported.

The project is a work-in-progress. See the [latest thoughts](thoughts.md).

## Coalton-Lisp Bridge

### Coalton within Lisp

Coalton value definitions come in two forms.

```commonlisp
;; Coalton
(define f v)
;; Lisp
f   ; an object (possibly a function) in the variable namespace
    ; can be lexically shadowed

;; Coalton
(define (f x ...) v)
;; Lisp
f    ; a function object in the variable namespace
     ; can be lexically shadowed
#'f  ; a function object in the function namespace
```

### Lisp within Coalton

Use the `coalton:lisp` macro to embed Lisp code inside of a Coalton program. Of course, Coalton will not be able to verify your Lisp code is correct. For example, the following function uses Common Lisp's `~R` format directive to write integers as strings.

```commonlisp
;; Coalton
(coalton-toplevel
  (declare integer-name (-> Integer String))
  (define (integer-name n)
    (lisp String
      (cl:format cl:nil "~R" n))))
```

## Examples

**You may want to check out the [library](src/library.lisp) for example code.** It represents the latest of what Coalton can do, and it also has a lot of definitions that the following depends on. (It is loaded by default as a part of Coalton.)

First we will load the system and get into `COALTON-USER`. Note that this package currently does not `:USE` the `COMMON-LISP` package, so you must qualify CL symbols manually.

```
CL-USER> (ql:quickload :coalton)
...
CL-USER> (in-package :coalton-user)
#<COMMON-LISP:PACKAGE "COALTON-USER">
COALTON-USER>
```

We already have a `Maybe` algebraic data type (ADT) defined in the library. We can immediately construct these values in Common Lisp. We use the convention in Lisp code to capitalize the first letter of constructors and type names. (Otherwise the capitalization has no significance.)
```commonlisp
COALTON-USER> (cl:list (Just 5) Nothing Nothing)
(#.(JUST 5) #.NOTHING #.NOTHING)
COALTON-USER> cl::(eq (second *) (third *))
COMMON-LISP:T
```

We can define a type-safe function `gg`. Definitions must occur within  `coalton-toplevel` forms.
```
COALTON-USER> (coalton-toplevel
                (define (gg x) (if x (left 1) (right x))))
GG
```

And we can use `gg` from Lisp.
```
COALTON-USER> (cl:funcall gg True)
#.(LEFT 1)
```

More conveniently, however, we can use the Coalton-to-Lisp bridge using the `coalton` macro.

```
COALTON-USER> (coalton (gg True))
#.(LEFT 1)
```

It's more convenient because it doesn't use `funcall`, and catches _compile-time type errors_!

```
COALTON-USER> (coalton (gg 1))

Type error: The types INTEGER and BOOLEAN don't match.
   [Condition of type COMMON-LISP:SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1008E40413}>)
```

We can inspect what Coalton inferred the type of `gg` to be using `type-of`. Note that, unlike in Common Lisp, `coalton:type-of` takes a symbol naming a global value.

```
COALTON-USER> (type-of 'gg)
(-> BOOLEAN (EITHER INTEGER BOOLEAN))
```

The Coalton library defines `Liszt`, whose name will surely change. It's a homogeneous list type, and as usual, we can construct values in Lisp.
```
COALTON-USER> (coalton-toplevel
               (define l (Kons (Just 1) (Kons Nothing (Kons (Just 2) Knil)))))
#.(KONS #.(JUST 1) #.(KONS #.NOTHING #.(KONS #.(JUST 2) #.KNIL)))
```

The type inferencer will deduce the type correctly.
```
COALTON-USER> (type-of 'l)
(LISZT (MAYBE INTEGER))
```

And it will also catch errors.
```
COALTON-USER> (coalton (Kons 1 2))

Type error: The types INTEGER and (LISZT INTEGER) don't match.
    [Condition of type SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1008E28413}>)
```

