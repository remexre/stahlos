Debug
=====

The Debug protocol exists for debugging purposes, as the name would indicate.

```scribble
global protocol Debug(role C, role S) {
	choice at C {
		Ping(Bytes) from C to S;
		Pong(Bytes) from S to C;
		do Debug(C, S);
	} or {
		DebugPrint(Bytes) from C to S;
		do Debug(C, S);
	} or {
		// The client can leave at any time.
	}
}
```

Messages
--------

### Ping

The body of a Ping message is sent back in the corresponding Pong message.

### Pong

The body of a Pong message is taken from the corresponding Ping message.

### DebugPrint

The body is output to port `0xe9`. In Bochs and QEMU, this causes it to be printed as a debug message. Furthermore, a breakpoint will be hit in Bochs (from an `xchg bx, bx` instruction) after the message is printed.
