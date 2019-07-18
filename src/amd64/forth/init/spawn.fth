MODULE

: evaluate-module ?( tag-addr --) DUP 8 + D@ SWAP #12 + D@ OVER - EVALUATE ;
: mb2-module-entrypoint ?( tag-addr --) RESET-TO-STD evaluate-module ;

: spawn-mb2-modules
  mb2-end mb2-tags ?DO
    I mb2-tag-type mb2-type-module = IF
      I 1 ['] mb2-module-entrypoint SPAWN
    THEN
    I mb2-tag-size
  +LOOP ;

END-MODULE( spawn-mb2-modules )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
