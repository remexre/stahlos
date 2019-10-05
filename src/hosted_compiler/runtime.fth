\ The runtime for a compiled Stahl program. This should really get compiled
\ once and have Stahl programs all share it, especially once the self-hosting
\ compiler is in use.

HEX
." main:State = " ' main:State . CR
." main:setup = " ' main:setup . CR
." main:loop = " ' main:loop . CR

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
