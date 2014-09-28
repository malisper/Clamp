Clamp
====

Common Lisp with Arc Macros and Procedures

Arc is an amazing programming language because of its brevity and
succinctness, but at the same time, it lacks some of the most basic
features of a programming language. It has neither a debugger nor a
module system. Common Lisp on the other hand has many of the
fundamentals that Arc lacks and much more (restarts, reader macros,
etc), but lacks the conciseness of Arc. Clamp is an attempt to bring
the powerful, but verbose, language of Common Lisp up to the terseness
of Arc.

There are currently two parts to Clamp. There is the core of Clamp,
which implements the utilities of Arc that are easily converted from
Arc to Common Lisp. The other part is the 'experimental' part. It
contains features of Arc that are not so easy to copy (ssyntax,
argument destructuring, etc).

The package :clamp (which is provided through the system :clamp)
exports not only the symbols that are new in Clamp, but also exports
most of the symbols from the :cl package. This is done so that it is
possible to shadow Common Lisp operators which do different things
than the Arc operators of the same names. By using Clamp in a package,
you are automatically using most of the symbols exported by :cl (some,
such as rplaca, have not been exported because use of them is
generally considered bad style).

The package :clamp-experimental (provided by the system :clamp-experimental)
works a little differently. It only exports the new symbols it
defines. In order to use both :clamp and :clamp-experimental, you will
have to use both and then handle the conflicts.

A lot of code has been taken from both the original Arc and Anarki.

If you need help loading the ASDF system, read this:
http://common-lisp.net/project/asdf/asdf/Quick-start-summary.html#Quick-start-summary
