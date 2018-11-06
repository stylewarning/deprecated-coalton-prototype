# Coalton

Coalton is a dialect of ML embedded in Common Lisp. It emphasizes practicality and interoperability with Lisp, and is intended to be a DSL that allows one to gradually make their programs safer.

The project is a work-in-progress. See the [latest thoughts](thoughts.md).

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
```
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
