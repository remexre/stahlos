Building
========

To build, run `make`. (To see all targets, `make help`.)

Building requires:

-	coreutils or equivalent
-	binutils, or at least a copy of `ld` and `strip` that can handle `elf64-x86-64` binaries
-	[GRUB](https://www.gnu.org/software/grub/), for `grub-file` and `grub-mkrescue`
-	[make](https://www.gnu.org/software/make/), to run `make`
-	[mdbook](https://github.com/rust-lang-nursery/mdBook), to build this documentation
-	[mtools](https://www.gnu.org/software/mtools/), for `grub-mkrescue`
-	[nasm](https://nasm.us/)
-	[xorriso](https://www.gnu.org/software/xorriso/), for `grub-mkrescue`
-	[OCaml](https://ocaml.org/), version 4.08.0 or later
-	[Dune](https://dune.build/)

Additionally, for other targets, the following dependencies apply:

-	[Bochs](http://bochs.sourceforge.net/), for the `run` target
-	a C compiler and headers, for the `test` and `utils` targets
-	[expect](https://core.tcl-lang.org/expect/index), for the `test` target
-	[gdb](https://www.gnu.org/software/gdb/), version 8.3 or higher, for the `debug` target
-	`objdump` (from binutils), for the `disas` target
-	[QEMU](https://www.qemu.org/), for the `run-qemu` and `test` targets
-	[watchexec](https://github.com/watchexec/watchexec), for the `watch` target

When in doubt, see `.travis/Dockerfile` in the source code -- it contains all the necessary dependencies to build and test StahlOS.

Installing
==========

`out/stahlos.img` is a disk image containing GRUB, configured to be able to be booted in the ways the `grub-mkrescue` was configured to be able to. (For official releases, this means BIOS boot and UEFI; depending on your system configuration, different methods may be supported.)

To run in QEMU, see the `run-qemu` target of the Makefile.

In theory, it should be possible to run this on a real machine. It requires AESNI.
