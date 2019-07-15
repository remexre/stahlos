: qemu-shutdown ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

traverse-mb2
check-modules
make-free-page-list
setup-himem
\ spawn-mb2-modules

\ TRY-RDSEED DEBUG

.( Done with init!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
