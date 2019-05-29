ByteOutputStream
================

The ByteOutputStream protocol is an abstraction for reading a stream of input bytes from an I/O device.

```scribble
global protocol ByteOutputStream(role C, role S) {
	choice at C {
		WriteBytes(OwnedBytes) from C to S;
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

### WriteBytes

### WriteBytesDone

### WriteEOF
