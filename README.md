Clamp
====

Common Lisp with Arc Macros and Procedures

Arc is a language which has many features that make it enjoyable to program in. At the same time it lacks some of the most basic fundamentals of a programming language. It has neither a debugger nor a module system. Clamp is an attemp to bring Common Lisp, which has both an amazing debugger and a module system, up to arc's level in succintness and brevity.

A lot of code is taken from both the original arc and anarki.

Defalias is a macro which allows one to easily give new names to procedures or macros already in common lisp.

TODO:
Add tests using some framework such as CLUnit.
Write the rest of the functions from arc.
Move some more code around and clean up.
Add functions from On Lisp.
Add type declarations to speed up code.
Figure out why defmemo gives a warning.