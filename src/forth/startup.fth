: MiB #20 RSHIFT D. ." MiB" ;

." Paged " TOTAL-PAGED-HIMEM MiB CR
." Free mem: " FREE-HIMEM MiB CR
." Used mem: " USED-HIMEM MiB CR

: main ;

.( Done with startup!)

.( Shutting down...)
:NONAME ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ; EXECUTE

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
