\ The runtime for a compiled Stahl program. This should really get compiled
\ once and have Stahl programs all share it, especially once the self-hosting
\ compiler is in use.

." main:State = " HEX ' main:State . CR
." main:setup = " HEX ' main:setup . CR
." main:loop = " HEX ' main:loop . CR

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
