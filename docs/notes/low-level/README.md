Low-Level Notes
===============

StahlOS runs entirely in ring 0 of long mode; the intention is to have a small base of assembly code (e.g. <10k instructions) that implements the Forth system, then to implement everything else on top of that. Since the kernel is the only binary on the system, and relatively little code is hand-written in Forth, all code that would be considered "user-mode" can be compiled so as to enforce process isolation.
