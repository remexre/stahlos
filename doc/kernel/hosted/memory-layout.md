# Memory Layout

In the hosted "kernel," the Forth interpreter is implemented on top of an array as a VM, with a single 128-bit register for the current process' PID.

```
0x0000 +-----------------------------------------------+
       | 256 nullable pointers to process table links  |
0x0800 +-----------------------------------------------+
       | Next PID to run                               |
0x0810 +-----------------------------------------------+
       | HERE pointer                                  |
0x0818 +-----------------------------------------------+
       | Init's Process Link                           |
0x0838 +-----------------------------------------------+
       | Init's Process Table                          |
0x0c38 +-----------------------------------------------+
       | Init's Source                                 |
       +-----------------------------------------------+
       | Free Memory                                   |
       +-----------------------------------------------+
```

A process table link just facilitates the process hashmap:

```
0x00 +-----------------------+
     | Next Link Pointer     |
0x08 +-----------------------+
     | Process Table Pointer |
0x10 +-----------------------+
     | PID                   |
0x20 +-----------------------+
```

## Process Table

```
0x0000 +---------------------+---------------------+---------------------+---------------------+
       | Dictionary Pointer  | Source Pointer      | Source Length       | Source Offset       |
0x0020 +---------------------+---------------------+---------------------+---------------------+
       | Instruction Pointer | Stack Length Left   | RStack Length Left  | Mailbox Pointer     |
0x0040 +---------------------+---------------------+---------------------+---------------------+
       | Reserved            | Reserved            | Reserved            | Reserved            |
0x0060 +---------------------+---------------------+---------------------+---------------------+
       | Reserved            | Reserved            | Reserved            | Reserved            |
0x0080 +---------------------+---------------------+---------------------+---------------------+
       |                                     Return Stack                                      |
0x0200 +---------------------+---------------------+---------------------+---------------------+
       |                                       Data Stack                                      |
0x0400 +---------------------+---------------------+---------------------+---------------------+
```
