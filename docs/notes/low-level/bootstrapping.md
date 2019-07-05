Bootstrapping
=============

The Forth implementation is bootstrapped so as to ease porting to new architectures (the 64-bit variants of ARM and RISC-V being the main candidates). The implementation is split into four parts.

Builtins
--------

Builtins are all code words, and must be (relatively laboriously) ported between systems. In general, these functions fall into four categories:

-	wrap a single instruction, e.g. `!` or `/*`
-	wrap a variable, e.g. `HERE`
-	twiddle compiler state, e.g. `[` or `IMMEDIATE`
-	strongly depend on the compiler internals, e.g. `(EXIT)`

Pseudobuiltins
--------------

Pseudobuiltins are defined as a standard Forth word defined with `:` would be. However, the parser does not yet exist (and is in fact written using pseudobuiltins), so they must be written by hand.

These mainly focus on getting the parser up and running, and providing enough compiler internals to semi-manually construct words.

Standard Library
----------------

This is currently `src/amd64/forth/std.f`. Words mentioned in the core word set of the Forth 2012 standard are generally defined here. This file should be portable between different architectures that share the same cell size, word layout, etc.

Init
----

This is currently `src/amd64/forth/init.f`. This file finishes initializing hardware, and begins loading startup programs (as specified as Multiboot2 modules).
