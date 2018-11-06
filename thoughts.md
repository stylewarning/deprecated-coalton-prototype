# ML in Common Lisp

_Robert Smith_

In this document, I meander about Lisp and ML, and hopefully get closer to a design for what ML might look like as a DSL in Lisp.

## Programming in the Large

Programs can get large. Along with that comes challenges in comprehension, modification, and maintenance. Many languages are either particularly good or particularly bad at providing ease in these areas for programmers. Flat assembly code is perhaps the worst in all of these. Nothing but the computer's instructions are explicit. Data structures or any structured understanding of control flow are totally opaque, maybe even lost, assembly code. It's difficult to navigate, extend, and debug a large program written in assembly.

Assembly is an obvious language to pick on, but once we reach higher level languages, it gets more difficult to express the subtleties involved in assessing the fitness of a programming language for writing large programs.

- Does the language manage names well?
  - Are there ways of creating new namespaces that won't likely collide with others'?
- Does the languages' implementation offer any static or dynamic guarantees of correctness?
  - Do we know that certain classes of errors are not possible?
- Does the language intrinsically, or the implementation explicitly, permit one to incrementally change programs?
- Does the language offer or allow to be expressed re-usable abstractions? What kinds?
- Is tool-driven inspection possible? Is language-driven introspection possible? To what extent?

There are many more questions we could ask that help characterize a language (or respective implementation) in terms of its ability to program in the large.

Common Lisp does pretty well when it comes to incremental changes, inspection, and introspection. Development environments, while not visually appealing, give a first-class experience in interactive software development.

Common Lisp does okay in terms of available abstractions. It does better at syntactic abstraction, okay at data abstraction, and poorly at module abstraction. Similarly, it does okay in terms of namespace management. Common Lisp namespaces, called _packages_, provide one level of naming indirection (i.e., every named thing lives in a constituent named package, so two names give the full ontology).

The question about guarantees is more complicated. The existence of a garbage collector ensures there won't be certain classes of memory errors. The strong-typedness of Common Lisp ensures that a program which has a type error cannot descend into a state that no longer respects the predictable semantics of the language. Common Lisp doesn't do so well when it comes to providing static guarantees, however. While the Python [CMUPython] compiler provides advanced type inference and analysis which helps find type errors statically, even in practice, it doesn't (and can't) provide any guarantee on its effectiveness or comprehensiveness. To add insult to injury, Python must work within the confines of the Common Lisp type system, which doesn't allow specifying even rank-1 polymorphic function or data types. (Common Lisp allows for, to a limited extent, both ad hoc and parametric polymorphism, but neither is specified at the type level.)

Depending on the nature of a large program in question, some of the above questions may be more important than others. If the large program is something like a sophisticated compiler, which operates on a rich and well-specified language with a plethora of intermediate representations, then static guarantees might rank higher in importance than, say, language-driven introspection.

Languages have been created the express purpose of building compilers and theorem provers for cutting-edge language research. In the 1970s, the language ML was created, which sprouted a handful of dialects, such as F# (developed by Microsoft), OCaml (developed by INRIA), and Standard ML (standardized by [SML]). These languages are applicative, functional languages with strong, static typing. They all allow for recursive, polymorphic data types and code. Compilers of any of the ML dialects essentially prove or disprove the soundness of the types of your programs, and often more. This is helpful when one makes a small change to the structure of a data structure (e.g., changing one's mind about representing a value as an `int`) and wants to be sure that such a change has not affected the (type-)correctness of the program, and if it has, identify every area in which it has.

Lisp isn't so bad for writing compilers. (In fact, I think it's quite good when you're not exactly sure what you're compiling _from_ and/or _to_.) After all, ML is known as "Lisp with types." But when it comes to programming (compilers) in the large, Lisp would be nicer were it closer to ML. Can Lisp afford to be as such?

## Lisp as a Substrate for Syntax

John Fodarero said "Lisp is the programmable programming language" [OnLisp]. In principle, new syntax can be created and existing syntax can be adapted in such a way that new semantics can be expressed. There is a spectrum to the viability of various syntactic abstractions. On the low end are simple syntaxes that are borne out of existing constructs. This sort of "syntactic programming" is done in various languages other than Lisp. For example, methods might be chained to create an example of a "readable" data pipeline, as in this pseudocode to compute the Riemann zeta function to a certain order:

```
zeta(s, n) := integers.keepIf(isPrime)
                      .take(n)
                      .each(x => 1 / (1 - 1/x^s))
                      .reduce(x, y => x * y)
                      .answer()
```

In this case, we've introduced no new syntax to the programming language itself, but rather, we've arranged for a cohesive set of patterns that are immediately recognizable to a human reader.

Another example common in many programming languages is to introduce syntax inside of strings. Perhaps the most popular example of this is with regular expressions. One can check whether a string is a sequence of `a`'s and `b`'s by matching said string against a regular expression written as `"(a|b)+"`. Here, we introduce meaning to regular expression syntax embedded inside of the string.

Lisp allows for a few higher levels of syntactic abstraction. The first level is abstraction of syntax over S-expressions using macros. A _macro_ is an arbitrary function that takes a list of forms and produces a form. One of the most prominent examples is Lisp's venerable iteration macro `loop`. The above zeta function calculation can be re-expressed as a usual imperative loop familiar to C or Python programmers: 

```
(loop :with z := 1
      :for x :from 1 :to n
      :when (primep x)
        :do (setf z (* z (/ (- 1 (expt x (- s))))))
      :finally (return z)))
```

While `loop` is built in to Common Lisp, it is not a "core construct" that requires special interpretation by the compiler. Instead, `loop` is defined as a macro which the compiler processes like any other. This code expands into a more primitive version of Common Lisp consisting of core constructs called _special forms_:

```
(BLOCK NIL
  (LET ((Z 1))
    (LET ((X 1) (LIMIT N))
      (TAGBODY
       :NEXT-LOOP
        (IF (> X LIMIT)
            (GO :END-LOOP))
        (IF (PRIMEP X)
            (SETQ Z (* Z (/ (- 1 (EXPT X (- S)))))))
        (SETQ X (1+ X))
        (GO :NEXT-LOOP)
       :END-LOOP
        (RETURN-FROM NIL Z)))))
```

Most would agree that the `loop` syntax is much easier to read than a tangle of tagbodies and gotos.

The user is permitted to create their own macros. For instance, some folks were dissatisfied by the baroqueness of `loop` and created an alternative syntax for iteration called `iterate` [Iterate]. As can be seen, macros are a very powerful feature, especially if you've acquired the subtle but nonetheless satisfying taste of S-expression meta-syntax. Without any additional means of syntactic abstraction, macros provide an enormous amount of syntactic flexibility to the user, and can lead to very succinct solutions to otherwise cumbersome-to-solve problems. Some further examples of macros can be found in [Alexa, Postmodern, CFFI]. 

We call S-expressions _meta-syntax_ because S-expressions specify a construction of data and only data. Additional syntactic rules, usually in the form of specific arrangements of S-expressions, must be added atop in order to give such S-expressions meaning, such as execution semantics. Lisp programmers reserve "meta-syntax" that falls out of the S-expression meta-syntax only for the most special of occasions. These special occasions call for the next level of syntactic abstraction: the modification of character-level syntax.

Character-level syntax can be changed by using reader macros. The user can specify that as the Lisp code is being read in for processing, if a certain character is seen, the parsing process should break out to a function that the user defines. Such functions associated to such characters are known as _reader macros_. We will show one example.

In Common Lisp, the full, standard S-expression syntax for an anonymous function that doubles its argument is:

```
(function (lambda (x) (* 2 x)))
```

The designers of Common Lisp had the sensibility to not require the user to write out this annoying `function` special operator [CLHS:Function] each time. They did so not by making the language more complicated, but rather by introducing a macro for the purpose of brevity, also called `lambda` [CLHS:MLambda]. The macro's sole purpose is to write out `function` for you:

```
(defmacro lambda (&whole form args &rest body)
  (declare (ignore args body))
  (function ,form))
```

With this, one can simply write:

```
(lambda (x) (* 2 x))
```

To someone accustomed to the likes of Haskell or ML might want an even more succinct notation than this. In the lambda calculus, we write $\lambda x.2x$. Since the dot `.` has plenty of meanings already, we might instead use the arrow $\lambda x\to 2x$, which is what Haskell does. In our code, we'll want `λ<var> -> <expr>` to be equivalent to `(lambda (<var>) <expr>)`. First, we define a function to parse these elements.

```
(defun lambda-reader (stream char)
  (declare (ignore char))
  (let ((arg   (read stream t nil t))   ; the lambda arguments <arg>
        (arrow (read stream t nil t))   ; the arrow "->" we expect
        (body  (read stream t nil t)))  ; the expression <expr>
    (assert (string= arrow "->"))
    `(lambda (,arg) (declare (ignorable ,arg)) ,body)))
```

This function tells us that if we have a stream of characters (likely the file of Lisp code being read), and we've encountered the character `char`, we should produce a `lambda` form, after reading the stream far enough to get the lambda arguments and the actual expression over which the lambda abstracts.

We can then tell Lisp that we should invoke this function any time we see a `λ` character.

```
(set-macro-character #\λ #'lambda-reader)
```

Now we are free to use this syntax as if it was a given language feature in the first place. We can even nest the lambda expressions.

```
CL-USER> (mapcar λx -> (* 2 x) '(1 2 3 4))
(2 4 6 8)
CL-USER> (defvar const λx -> λy -> x)
CONST
CL-USER> (funcall (funcall const 5) 100)
5
```

With enough reader macros, one can create entirely new languages that neither look nor act like Lisp. For instance, Vacietis [Vacietis] makes bonafide C code with C syntax able to be processed by Lisp by using reader macros. Another example is the somewhat recently popular syntax for interpolated strings, which can be accomplished with the `#?` reader macro, as in  `#?"I have ${bank-balance} dollars."` [Interpol].

Syntactic abstraction seems fantastic, but its value does ultimately depend on what sort of semantics you can assign to the syntax. A Turing tarpit argument might be something along the lines of

> Well, Common Lisp is Turing complete, so one can express any arbitrary computable semantics.

It's true, but not very qualitatively helpful. Instead, we can take a look at the core of the Lisp language.

The core of the Lisp language is relatively rich. We can see that the special operators as listed in CLHS section 3.1.2.1.2.1 [CLHS:SpecialForms] provide all manner of low-level control flow, evaluation control, environment control, and data flow control:

```
Control Flow:
  BLOCK CATCH GO IF PROGN RETURN-FROM TAGBODY THROW
  UNWIND-PROTECT
Evaluation Control:
  EVAL-WHEN LOAD-TIME-VALUE MACROLET QUOTE SYMBOL-MACROLET
Environment Control:
  FLET FUNCTION LABELS LET LET* LOCALLY PROGV SETQ THE
Data Flow Control:
  MULTIPLE-VALUE-CALL MULTIPLE-VALUE-PROG1
```

Pile on top of this the existence of first-class functions, applicative-order evaluation, and a large standard library, and one is presented with a relatively powerful target language for the expression of semantics. With that said, not all constructs can be expressed. For instance, there is no portable way to express a "computed goto" [WikiGoto, WikiBranchTable] that works in constant time, constant memory, and respects the lexical environment. (In the spirit of Turing equivalence, however, we can simulate the functionality.)

_To Be Continued..._

## [Dumping Ground of Ideas]

Name: "Coalton", a play on MLton with CL in the name. I don't want to risk a name like CLton where everybody will mispronounce it. :)

Lisp implementation package name: `coalton-impl`

This contains all of the internal functionality. Manages the type (and other metadata) database. Contains parsers, compilers, inference algorithms, etc.

Lisp library package name: `coalton`, no nicknames

Lisp user package name: `coalton-user`, no nicknames

### Syntax

Syntax should still be S-expressions, perhaps with a few minor reader extensions.

Maybe ML tuples should be marked with square brackets `[...]` since parentheses will get too confusing?

### Interoperability

Interoperability in both directions should be a primary concern to the fullest practical extent. It should be possible to mix and splice whatever can be.

* Should still make use of the package system.

`(coalton:lisp type lisp-form)`: embed Lisp in Coalton

`(coalton:coalton coalton-form)`: embed Coalton in Lisp

### Function/Value Namespace

Coalton should probably be a Lisp-1 dealio. Function names and value names shouldn't be distinguished. Is this too inconvenient? Lisp doesn't have global lexical variables. How do these play in with existing function names? This should be addressed ASAP.

### Currying?

What does a curried definition look like?

### Giant Macro?

Coalton definitions will live in a toplevel macros. These will have letrec-like scope.

```
(coalton:coalton-toplevel
  ...
  data defines, function defines, etc.
  ...
)
```

Can we get away with piecemeal macros?

### ASDF File Type

Entire Coalton files could be written in files and we could create an ASDF file type (?) allowing for

```
(asdf:defsystem #:foo
  :serial t
  :components ((:file "lisp-file")          ; lisp code
               (:coalton "coalton-file")))  ; coalton code
```

What about post-compilation-unit hooks?

### Function Definitions

no real ideas here, depends more or less how we want to define pattern matching, whether we want guards, how we deal with annotations on variable bindings, etc.

```
(coalton:declare len (-> (list a) integer))
(coalton:define (len x)
  (match x
    nil         => 0
    (cons x xs) => (+ 1 (len xs)))))
```

### Unsafe Lisp-in-ML Splicing

There should be a way to splice raw Lisp code unsafely into Coalton code using a `lisp` construct. This may deserve special character syntax, but that should be dictated by Real World usage, not speculation.

One example of a `(lisp <type> <lisp-form>)` syntax:

```
(coalton:type f (-> (* string string) integer))
(coalton:define (f x y)
  (length (lisp string (concatenate 'string x y))))
```

This could be **unsafe**! Coalton may as well assume that the type is correct.

### Safe ML-in-LIsp Splicing

There should be a way to splice Coalton code into Lisp code using the `ml` construct. If `speed > safety`, then (automatic) dynamic type checks are done.

```
(coalton:type f (-> integer integer))
(defun foo (x)
  (+ 2 (ml f x))) ; will check (INTEGERP X)
```

### Bless

Certain Lisp functions should be able to be "blessed" into Coalton.

```
(coalton:bless cl:sin (-> single-float single-float))
```

How should Lisp polymorphic functions be blessed? Can they be? Should they be? The function `cl:length` is maybe a good operative example. One possibility is just to monomorphize:

```
(defun string-length (x)
  (declare (type string x))
  (the integer (length x)))

(coalton:bless string-length (-> string integer))
```

Unrelated note, maybe a special `coalton:bless` isn't needed, and one should just use `coalton:type`.

How about type specialization? If Coalton detects the argument of `cl:length` is a string, it'll monomorphize automatically, or something? This is overloading/ad hoc polymorphism.

### Regulate

Certain Lisp functions should be able to be "regulated" to be Coalton compatible. This means some of the freeness permitted by Lisp should be locked down by Coalton. For instance, the `stringp` function is permitted to return any manner of generalized boolean. Maybe Coalton has `true` and `false`. We should make it easy to use `stringp` in Coalton.

```
(regulate cl:stringp (-> string (^ boolean)))
```

This makes references to `cl:stringp` in Coalton code actually refer to a regulated (but otherwise semantically compatible) form of `cl:stringp`. The `^` indicates that such an argument needs to be canonically downconverted as if by `(lambda (x) (if x true false))`.

Rationale: People will just want to use `stringp` and not Coalton's fancy, re-defined version of it.

## References

[CMUPython] Paper: The Python Compiler for Common Lisp by Robert MacLachlan. www.cs.cmu.edu/~ram/pub/lfp.ps

[SML] Book: The Definition of Standard ML (Revised) by Milner, Tofte, Harper, MacQueen

[OnLisp] Book: On Lisp by Paul Graham.

[Iterate] Website: https://common-lisp.net/project/iterate/

[Alexa] Website: https://github.com/rigetticomputing/alexa

[Postmodern] Website: https://github.com/marijnh/Postmodern

[CFFI] Website: The Common Foreign Function Interface https://common-lisp.net/project/cffi/

[CLHS:Function] CLHS: http://clhs.lisp.se/Body/s_fn.htm

[CLHS:MLambda] CLHS: http://www.clhs.lisp.se/Body/m_lambda.htm

[Vacietis] Website: https://github.com/vsedach/Vacietis

[Interpol] Website: http://edicl.github.io/cl-interpol/

[CLHS:SpecialForms] CLHS: http://clhs.lisp.se/Body/03_ababa.htm

[WikiGoto] Wikipedia: https://en.wikipedia.org/wiki/Goto#Computed_GOTO

[WikiBranchTable] Wikipedia: https://en.wikipedia.org/wiki/Branch_table