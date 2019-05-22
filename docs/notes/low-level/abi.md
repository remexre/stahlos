ABI
===

The pervasive use of Forth throughout the system makes the ABI less important for writing user code. Nevertheless, here it is:

-	`rbx` -- Top slot on Parameter Stack
-	`rsi` -- Forth instruction pointer
-	`rsp` -- Parameter Stack pointer
-	`rbp` -- Return Stack pointer
-	`r14` -- Top of Parameter Stack
-	`r15` -- Top of Return Stack
-	`rax`, `rcx`, `rdx`, `rdi` -- Scratch (not preserved across yields)
-	`r8`\-`r13` -- Scratch (not preserved across calls or yields)

The segment registers (cs, ds, es, fs, gs) should always have the same values, `0x08` for cs, `0x10` for the rest.
