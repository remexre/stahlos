StahlNet
========

Since writing a TCP stack is annoying, and supporting application-layer protocols is even more so, StahlNet exists. StahlNet is a UDP-based lightweight networking system. "Eventually," StahlOS' IPC primitives will be extended to be able to send over StahlNet. Since the StahlNet protocol has simple semantics, writing implementations in other languages is fairly simple.

Implementations
---------------

-	Rust: [stahlnet-rs](https://github.com/remexre/stahlnet-rs)
