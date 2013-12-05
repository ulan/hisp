Hisp
====

This repository is a playground for writing Lisp interpreters in Haskell. As a
specification for Lisp I use the ["Roots of Lisp"](http://www.paulgraham.com/rootsoflisp.html) paper, which closly follows the original paper of John McCarty.

The only part where I diverge from the specification is representation of boolean literals. In the paper the true value is represented as `'t` and the false value is represented as `'()`. I use atoms `true` and `false` instead since they are more symmetric and readable.

Hisp0 is the simplest interpreter I could come up with. It follows the spirit of the evaluator presented in the paper: it has dynamic scoping, no error handling, and can crash or hang on bad lisp programs. 

Hisp1 is more advanced with lexical scoping and error reporting.

