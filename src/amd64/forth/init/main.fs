: qemu-shutdown ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

traverse-mb2
check-modules
make-free-page-list
setup-himem
\ spawn-mb2-modules

: cmos ?( u --) DUP ." 0x" H. $70 OUTB $71 INB ." - " D. CR ;
0 cmos
2 cmos
4 cmos
6 cmos
7 cmos
8 cmos
9 cmos

.( Done with init!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
