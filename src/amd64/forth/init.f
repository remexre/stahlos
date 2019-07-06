: qemu-shutdown ?( --)
  $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

1 IPB CONSTANT mb2 ?( The multiboot2 info structure's address)
: mb2-each-tag ?( xt --)
  mb2 D@ mb2 +
  mb2 8 +
  DO
    I SWAP DUP >R EXECUTE R>
    SWAP IF LEAVE THEN
    I 4 + D@ ALIGN-TO-CELL
  +LOOP DROP ;

:NONAME ?( addr -- flag)
  DUP D@ 6 = IF
    . TRUE
  ELSE
    DROP FALSE
  THEN ; DROP \ mb2-each-tag

:NONAME 5 0 DO I . LOOP ; EXECUTE
.( going from first to second test)
:NONAME 5 0 DO I . I 3 = IF DEBUG LEAVE THEN LOOP ; EXECUTE

.( Done with init.f!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
