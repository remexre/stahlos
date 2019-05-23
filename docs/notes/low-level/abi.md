ABI
===

The pervasive use of Forth throughout the system makes the ABI less important for writing user code. Nevertheless, here it is:

-	`rbx` -- Top slot on Parameter Stack
-	`rsi` -- Forth instruction pointer
-	`rsp` -- Parameter Stack pointer
-	`rbp` -- Return Stack pointer
-	`r13` -- Bottom of Parameter Stack
-	`r14` -- Bottom of Return Stack
-	`r15` -- User Pointer
-	`rax`, `rcx`, `rdx`, `rdi` -- Scratch (not preserved across yields)
-	`r8`\-`r12` -- Scratch (not preserved across calls or yields)

The segment registers (`cs`, `ds`, `es`, `fs`, `gs`) should always have the same values, `0x08` for `cs`, `0x10` for the rest.

The User Pointer points to a structure that looks like:

```
   0 +--------------------+
     | Dictionary Pointer |
   8 +--------------------+
     |        Flags       |
  16 +--------------------+
     |   Start of Source  |
  24 +--------------------+
     |  Length of Source  |
  32 +--------------------+
     |                    |
    ...     Reserved     ...
     |                    |
 224 +--------------------+
     |     Stored RBX     |
 232 +--------------------+
     |     Stored RSI     |
 240 +--------------------+
     |     Stored RSP     |
 248 +--------------------+
     |     Stored RBP     |
 256 +--------------------+
     |                    |
     |    Return Stack    |
     |     (32 words)     |
     |                    |
 512 +--------------------+
     |                    |
     |                    |
     |                    |
     |   Parameter Stack  |
     |      (64 words)    |
     |                    |
     |                    |
     |                    |
1024 +--------------------+
```

The flags are:

```
MSB
00000000 00000000 00000000 00000000
00000000 00000000 00000000 0000000B
                                LSB
```

|   | Name     | Description          |
|---|----------|----------------------|
| 0 | Reserved | Should be 0          |
| B | Base     | 0 = decimal, 1 = hex |
