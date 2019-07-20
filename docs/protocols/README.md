Protocols
=========

This section documents the messages and protocols used throughout StahlOS.

Messages
--------

StahlOS uses message-passing as the communication primitive between tasks. Each message is conceptually just the sender's task ID, a type code, and an arbitrary-length buffer of bytes. Since this body doesn't inherently have any structure, it is necessary for both sender and receiver to agree on the format of the messages sent for each type code.

The following messages are currently specified:

| Message                                                | Type Code            |
|--------------------------------------------------------|----------------------|
| [DebugPrint](debug.md#debugprint)                      | `0x66c379ab0b7196ef` |
| [Directory](todo.md)                                   | `0xc0829ee663ced008` |
| [Ping](heartbeat.md#ping)                              | `0x0000000000000000` |
| [Pong](heartbeat.md#pong)                              | `0xffffffffffffffff` |
| [ReadBytesData](byte-input-stream.md#readbytesdata)    | `0x9d373588abfc316c` |
| [ReadBytes](byte-input-stream.md#readbytes)            | `0x453fbbee6bd6a904` |
| [ReadEOF](byte-input-stream.md#readeof)                | `0x66822efd36030ee1` |
| [Register](todo.md)                                    | `0x2c15b9b13a3e2a80` |
| [WriteBytesDone](byte-output-stream.md#writebytesdone) | `0xfa2df296436c000b` |
| [WriteBytes](byte-output-stream.md#writebytes)         | `0xa80817401471655f` |
| [WriteEOF](byte-output-stream.md#writeeof)             | `0xd43aa3d9caaf192e` |

(Note: In general, type codes are assigned as FNV1a hashes of the name of the message.)

Protocols
---------

The higher-level behavioral interfaces between tasks are referred to as protocols. Currently, protocols are documented using pseudo-[Scribble](http://www.scribble.org/). In the future, a custom language may be used instead that takes ownership into account, and can generate Forth or Stahl code corresponding to the protocol.
