# Coalton

Coalton is a dialect of ML embedded in Common Lisp. It emphasizes
practicality and interoperability with Lisp, and is intended to be a
DSL that allows one to gradually make their programs safer.

The project is a work-in-progress. Check out below for things that
sort of work.

See the [latest thoughts](thoughts.md).

## Examples

Here are some transcripts of neat things that sort of work. Check out
the [library](src/library.lisp) for example code that the following
depends on.

First we will load the system and get into `COALTON-USER`. Note that
this package currently does not `:USE` the `COMMON-LISP` package, so
you must qualify symbols manually.

```
CL-USER> (ql:quickload :coalton)
...
CL-USER> (in-package :coalton-user)
#<COMMON-LISP:PACKAGE "COALTON-USER">
COALTON-USER>
```

We already have a `Maybe` AST defined in the library. We can
immediately construct these values in Common Lisp.
```
COALTON-USER> (cl:list (Just 5) Nothing Nothing)
(#<JUST {100B47F7F3}> #<NOTHING {100B0481D3}> #<NOTHING {100B0481D3}>)
COALTON-USER> (cl:describe (cl:first cl:*))
#<JUST {100B47F7F3}>
  [standard-object]

Slots with :INSTANCE allocation:
  VALUE                          = #(5)
; No value
COALTON-USER> cl::(eq (second *) (third *))
COMMON-LISP:T
```

We can define a type-safe function `gg`.
```
COALTON-USER> (coalton
                (define (gg x) (if x (left 1) (right x))))
GG
```

And we can use `gg` from Lisp.
```
COALTON-USER> (cl:funcall gg cl:t)
#<LEFT {100B89E863}>
```

We can inspect what Coalton inferred the type of `gg` to be. We use
the package `COALTON-IMPL` to access this functionality.
```
COALTON-USER> (coalton-impl::unparse-type
               (coalton-impl::var-derived-type 'gg))
(-> BOOLEAN (EITHER INTEGER BOOLEAN))
```

The Coalton library defines `Liszt`, whose name will surely
change. It's a homogeneous list type, and as usual, we can construct
values in Lisp.
```
COALTON-USER> (Kons 1 (Kons 2 (Kons 3 Knil)))
#<KONS {100B9B54A3}>
```

The type inferencer will deduce the type correctly.
```
COALTON-USER> (coalton-impl::unparse-type
               (coalton-impl::derive-type
                (coalton-impl::parse-form
                 '(Kons (Left 1) (Kons (Right 2) (Kons (Left 3) Knil))))))
(LISZT (EITHER INTEGER INTEGER))
```

And it will also catch errors.
```
COALTON-USER> (coalton-impl::unparse-type
               (coalton-impl::derive-type
                (coalton-impl::parse-form
                 '(Kons 1 2))))

Type error: Type mismatch: INTEGER and (LISZT INTEGER)
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1008E28413}>)
```