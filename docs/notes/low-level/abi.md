ABI
===

The pervasive use of Forth throughout the system makes the ABI less important for writing user code. Nevertheless, here it is:

-	`rbx` -- Top slot on Parameter Stack
-	`rsi` -- Forth instruction pointer
-	`rsp` -- Parameter Stack pointer
-	`rbp` -- Return Stack pointer
-	`r13` -- Bottom of Parameter Stack
-	`r14` -- Bottom of Return Stack
-	`r15` -- Process Pointer
-	`rax`, `rcx`, `rdx`, `rdi` -- Scratch (not preserved across yields)
-	`r8`\-`r12` -- Scratch (not preserved across calls or yields)

The segment registers (`cs`, `ds`, `es`, `fs`, `gs`) should always have the same values, `0x08` for `cs`, `0x10` for the rest.

The Process Pointer points to the Process Area (see [Forth Structures](../../forth/structures.md#process-area)).

When a call is performed, `rax` must contain the address of the code field of the word being called.
