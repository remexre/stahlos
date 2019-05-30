Terminal
========

The Terminal protocol facilitates interactions between a terminal and a TUI process.

```scribble
global protocol Terminal(role C, role S) {
	choice at C {
		ReadBytes(usize) from C to S;
		choice at S {
			ReadBytesData(bytes) from S to C;
			do Terminal(C, S);
		} or {
			ReadEOF() from S to C;
		}
	} or {
		WriteBytes(Bytes) from C to S;
		choice at S {
			WriteBytesDone() from S to C;
			do ByteOutputStream(C, S);
		} or {
			WriteEOF() from S to C;
		}
	} or {
		// The client can leave at any time.
	}
}
```

Messages
--------
