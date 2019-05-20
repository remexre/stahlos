ABI
===

The pervasive use of Forth throughout the system makes the ABI less important for writing user code. Nevertheless, here it is:

```
rbx -- top slot on stack
rsi -- forth instruction pointer
rsp -- parameter stack pointer
rbp -- return stack pointer
rdi -- forth flags

scratch (not preserved across yield): rax, rcx, rdx
scratch (not preserved across calls): r8-r15
```

The segment registers (cs, ds, es, fs, gs) should always have the same values, `0x08` for cs, `0x10` for the rest.
