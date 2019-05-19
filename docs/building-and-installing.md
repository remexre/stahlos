Building and Installing
=======================

To build, run `make`. (To see all targets, `make help`.)

Building requires:

-	coreutils or equivalent
-	binutils, or at least a copy of `ld` and `strip` that can handle `elf64-x86-64` binaries
-	[GRUB](https://www.gnu.org/software/grub/), for `grub-file` and `grub-mkrescue`
-	[mdbook](https://github.com/rust-lang-nursery/mdBook), to build this documentation
-	[nasm](https://nasm.us/)

Additionally, for other targets, the following dependencies apply:

-	Bochs, for the `run` target
-	`objdump` (from binutils), for the `disas` target
-	QEMU, for the `run-qemu` target
-	watchexec, for the `watch` target
