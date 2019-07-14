Heartbeat
=========

The Heartbeat protocol is used to determine if a given process is "up and responsive."

```scribble
global protocol Heartbeat(role C, role S) {
	choice at C {
		Ping(bytes) from C to S;
		Pong(bytes) from S to C;
		do Heartbeat(C, S);
	}
}
```

Messages
--------

### Ping

The body of a Ping message is sent back in the corresponding Pong message.

### Pong

The body of a Pong message is taken from the corresponding Ping message.
