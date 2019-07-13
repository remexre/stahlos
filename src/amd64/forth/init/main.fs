: qemu-shutdown ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

traverse-mb2
mb2-module-check
make-free-page-list
page-pages-to-himem
." Paged " MAX-PAGED-HIMEM-ADDR @ MIN-PAGED-HIMEM-ADDR @ - #20 RSHIFT D. ." MiB" CR

.( Done with init!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
