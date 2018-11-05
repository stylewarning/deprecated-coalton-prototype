# Coalton

Coalton is a dialect of ML embedded in Common Lisp. It emphasizes
practicality and interoperability with Lisp, and is intended to be a
DSL that allows one to gradually make their programs safer.

The project is a work-in-progress. Check out below for things that
sort of work.

See the [latest thoughts](thoughts.md).

## Examples

Here are some transcripts of neat things that sort of work.

Defining a new ADT.
```
COALTON-IMPL> (coalton:coalton
                (coalton:define-type (Either a b)
                  (Left a)
                  (Right b)))
EITHER
```

We can use this ADT from Lisp.
```
COALTON-IMPL> (list (left 5) (right (left 1)))
(#<LEFT {100C5C2FC3}> #<RIGHT {100C63D433}>)
COALTON-IMPL> (describe (first *))
#<LEFT {100C686733}>
  [standard-object]

Slots with :INSTANCE allocation:
  VALUE                          = 5
```

We can define a type-safe function `gg`.
```
COALTON-IMPL> (coalton:coalton
                (coalton:define (gg x) (coalton:if x (left 1) (right x))))
GG
```

And we can use `gg` from Lisp.
```
COALTON-IMPL> (funcall gg t)
#<LEFT {100C681223}>
```

We can inspect what Coalton inferred the type of `gg` to be:
```
COALTON-IMPL> (unparse-type (var-derived-type 'gg))
(COALTON:-> COALTON:BOOLEAN (EITHER COALTON:INTEGER COALTON:BOOLEAN))
```

Defining a homogeneous list type.
```
COALTON-IMPL> (coalton:coalton
                (coalton:define-type (Liszt t)
                  Knil
                  (Kons t (Liszt t))))
LISZT
```

As usual, we can construct the values in Lisp.
```
COALTON-IMPL> (kons 1 (kons 2 (kons 3 knil)))
#<KONS {1003E5BD43}>
```

The type inferencer will deduce the type correctly.
```
COALTON-IMPL> (unparse-type
               (derive-type
                (parse-form
                 '(Kons (Left 1) (Kons (Right 2) (Kons (Left 3) Knil))))))
(LISZT (EITHER COALTON:INTEGER COALTON:INTEGER))
```

And it will also catch errors.
```
COALTON-IMPL> (unparse-type
               (derive-type
                (parse-form
                 '(Kons 1 (Kons 2 (Kons 3 4))))))

Type error: Type mismatch: COALTON:INTEGER and (LISZT COALTON:INTEGER)
   [Condition of type SIMPLE-ERROR]

Restarts:
 0: [RETRY] Retry SLIME REPL evaluation request.
 1: [*ABORT] Return to SLIME's top level.
 2: [ABORT] abort thread (#<THREAD "repl-thread" RUNNING {1008E28413}>)
```