ByteInputStream
===============

The ByteInputStream protocol is an abstraction for reading a stream of input bytes from an I/O device.

```scribble
global protocol ByteInputStream(role C, role S) {
	choice at C {
		ReadBytes(usize) from C to S;
		choice at S {
			ReadBytesData(bytes) from S to C;
			do ByteInputStream(C, S);
		} or {
			ReadEOF() from S to C;
		}
	} or {
		// The client can leave at any time.
	}
}
```

Messages
--------

### ReadBytes

### ReadBytesData

### ReadEOF
