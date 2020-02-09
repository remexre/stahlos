AArch64 ABI
===========

Ranges are inclusive.

```
x0-x9 - Arguments, Temporary Variables (Caller-Saved)

x10 - Top Value of Parameter Stack
x11 - Parameter Stack Base Pointer
x12 - Current Depth of Parameter Stack (in bytes)
x13 - Max Depth of Parameter Stack (in bytes)

x14 - Top Value of Return Stack
x15 - Return Stack Base Pointer
x16 - Current Depth of Return Stack (in bytes)
x17 - Max Depth of Return Stack (in bytes)

x18 - Forth Instruction Pointer
x19 - Process Table Pointer

x20-x29 - Reserved

x30 - Link Register
```

Process Table
-------------

```
0x0000 +---------------------+---------------------+---------------------+---------------------+
       | Dictionary Pointer  | Source Pointer      | Source Length       | Source Offset       |
0x0020 +---------------------+---------------------+---------------------+---------------------+
       | Mailbox Pointer     | Flags               | Reserved            | Reserved            |
0x0040 +---------------------+---------------------+---------------------+---------------------+
       | Instruction Pointer | Stack Depth         | RStack Depth        | Reserved            |
0x0060 +---------------------+---------------------+---------------------+---------------------+
       | Reserved            | Reserved            | Reserved            | Reserved            |
0x0080 +---------------------+---------------------+---------------------+---------------------+
       | Reserved            |                                                                 |
       +---------------------+                                                                 |
       |                                     Return Stack                                      |
       |                                                                                       |
       |                                                                                       |
0x0200 +---------------------+---------------------+---------------------+---------------------+
       | Scratch Space       |                                                                 |
       +---------------------+                                                                 |
       |                                       Data Stack                                      |
       |                                                                                       |
       |                                                                                       |
0x0400 +---------------------+---------------------+---------------------+---------------------+
```

Process Flags
-------------

```
MSB  LSB
0000000M
```

|   | Name     | Description                |
|---|----------|----------------------------|
| 0 | Reserved | Should be 0                |
| M | Mode     | 0 = compile, 1 = interpret |

Word Header
-----------

```
      0x00 +-------------------------------+
           |           Next Word           |
      0x08 +-------+-------+---------------+
           | Flags |  Len  |      Name    ...
Len + 0x0a +-------+-------+---------------+
           |              CFA             ...
           +-------------------------------+
           |              PFA             ...
           +-------------------------------+
```

The flags are:

```
MSB  LSB
0000000I
```

|   | Name      | Description               |
|---|-----------|---------------------------|
| 0 | Reserved  | Should be 0               |
| I | Immediate | 0 = normal, 1 = immediate |
