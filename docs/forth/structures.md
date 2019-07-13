Forth Structures
================

Words
-----

```
     0 +-------------------------------+  ---+
       |           Next Link           |     |
     8 +-------------------------------+     |
       |         Documentation         |     +-- Header
    16 +-------+-------+---------------+     |
       | Flags |  Len  |     Name     ...    |
24+Len +-------+-------+---------------+  ---+
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

Process Area
------------

```
   0 +--------------------+--------------------+--------------------+--------------------+
     |     Process ID     |   Start of Source  |  Length of Source  |     Input Offset   |
  32 +--------------------+--------------------+--------------------+--------------------+
	 |   Dictionary Head  |        Flags       |   Head of Mailbox  |   Tail of Mailbox  |
  64 +--------------------+--------------------+--------------------+--------------------+
	 |      ABORT xt      |        BP xt       |       EMIT xt      |       QUIT xt      |
  96 +--------------------+--------------------+--------------------+--------------------+
    ...                                    Reserved                                     ...
 224 +--------------------+--------------------+--------------------+--------------------+
     |      Reserved      |     Stored RSI     |     Stored RSP     |     Stored RBP     |
 256 +--------------------+--------------------+--------------------+--------------------+
     |                                                                                   |
     |                                    Return Stack                                   |
     |                                     (32 words)                                    |
     |                                                                                   |
 512 +--------------------+--------------------+--------------------+--------------------+
     |                                                                                   |
     |                                                                                   |
     |                                                                                   |
     |                                  Parameter Stack                                  |
     |                                     (64 words)                                    |
     |                                                                                   |
     |                                                                                   |
     |                                                                                   |
1024 +--------------------+--------------------+--------------------+--------------------+
```

The flags are:

```
MSB                                                                 LSB
00000000 00000000 00000000 00000000 00000000 00000000 00000000 000000SB
```

|   | Name     | Description                |
|---|----------|----------------------------|
| 0 | Reserved | Should be 0                |
| B | Base     | 0 = decimal, 1 = hex       |
| S | State    | 0 = interpret, 1 = compile |

GC Allocation
-------------

```
     0 +---------+
       |   Len   |
     8 +---------+
       | Mark XT |
    16 +---------+
       |  Flags  |
    24 +---------+
       |  Data   |
24+Len +---------+
       | Padding |
       +---------+
```

The Mark XT is the XT of a Forth word (the "mark word") that is invoked with the address of the data. For each GC pointer in data, the word should set the target's Marked flag, then call the target's mark word the the address as an argument. The `MARK-CHILD` word can help here.

The flags are:

```
MSB                                                                 LSB
00000000 00000000 00000000 00000000 00000000 00000000 00000000 0000000M
```

|   | Name     | Description        |
|---|----------|--------------------|
| 0 | Reserved | Should be 0        |
| M | Marked   | 0 = dead, 1 = live |

The allocation is padded such that the *total length*, i.e. `24+Len+PaddingLen`, is divisible by 8. Allocations must also be aligned to 8.
