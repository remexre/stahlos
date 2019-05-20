ABI
===

The pervasive use of Forth throughout the system makes the ABI less important for writing user code. Nevertheless, here it is:

-	`rbx` -- Top slot on Parameter Stack
-	`rsi` -- Forth instruction pointer
-	`rsp` -- Parameter Stack pointer
-	`rbp` -- Return Stack pointer
-	`rdi` -- Forth flags
-	`rax`, `rcx`, `rdx` -- Scratch (not preserved across yield)
-	`r8`\-`r15` -- Scratch (not preserved across calls)

The Forth flags are arranged as follows:

```
63         5      0
+----------+------+
| RESERVED | Base |
+----------+------+
```

The segment registers (cs, ds, es, fs, gs) should always have the same values, `0x08` for cs, `0x10` for the rest.
