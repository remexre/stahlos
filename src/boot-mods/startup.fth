S" startup" SET-PROCESS-NAME

." Startup is PID " PID H. ." with arguments: " TYPELN

: MiB #20 RSHIFT D. ." MiB" ;

."    Paged: " TOTAL-PAGED-HIMEM MiB CR
." Free mem: " FREE-HIMEM MiB CR
." Used mem: " USED-HIMEM MiB CR

YIELD
.( Done with startup!)
\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
