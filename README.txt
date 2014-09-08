Clamp
====

Common Lisp with Arc Macros and Procedures

Arc is a language which has many features that make it enjoyable to
program in. At the same time it lacks some of the most basic
fundamentals of a programming language. It has neither a debugger nor
a module system. Clamp is an attempt to bring Common Lisp, which has
many features that Arc lacks (debugger, packages, restarts, etc), up
to arc's level in succintness and brevity.

Right now Clamp is in its own package which exports not only the
symbols needed for Clamp, but almost all of the symbols originally
exported by the Common-Lisp package as well. There are two reasons for
exporting the Common-Lisp symbols. First of all it allows for symbols
that are already used in Common-Lisp to be shadowed and replaced with
an Arc version (ie let). Secondly it makes using the Clamp package
much easier. Instead of needing to use both Common-Lisp and Clamp and
handle the conflicting symbol names, one only needs to import Clamp.

So far a lot of the basic utilities from Arc have been implemented.
The [...] syntax for literal functions has been implemented as a
reader macro. Many of the original Common Lisp symbols (if, let, case,
etc) have been shadowed and replaced. The file package.lisp contains
the details for which symbols have been shadowed. To see what has been
implemented so far look at package.lisp and see which symbols are
exported.

A lot of code has been taken from both the original arc and anarki.

If you need help loading the asdf package, read this:
http://common-lisp.net/project/asdf/asdf/Quick-start-summary.html#Quick-start-summary

TODO:
Write the rest of the utilities from arc.
Add utilities from On Lisp.
Move some more code around and clean up.
Add type declarations to speed up code.
