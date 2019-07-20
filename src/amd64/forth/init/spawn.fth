MODULE

: module-tag>cmdline ?( tag-addr -- addr len) #16 + CSTR>STR ;
: module-tag>data ?( tag-addr -- addr len)
  DUP 8 + D@ SWAP #12 + D@ OVER - ;
: evaluate-module ?( tag-addr --)
  DUP module-tag>cmdline ROT module-tag>data EVALUATE ;
: mb2-module-entrypoint ?( tag-addr --)
  RESET-TO-STD
  ['] ABORT-DEFAULT IS-ABORT
  ['] INT3          IS-BP
  ['] EMIT-DEFAULT  IS-EMIT
  ['] DIE           IS-QUIT
  evaluate-module ;

: spawn-mb2-modules
  mb2-end mb2-tags ?DO
    I mb2-tag-type mb2-type-module = IF
      I 1 ['] mb2-module-entrypoint SPAWN
    THEN
    I mb2-tag-size
  +LOOP ;

END-MODULE( spawn-mb2-modules )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
