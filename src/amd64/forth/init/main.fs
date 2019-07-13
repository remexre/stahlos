: qemu-shutdown ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

traverse-mb2
mb2-module-check
make-free-page-list
page-pages-to-himem

: MiB #20 RSHIFT D. ." MiB" ;
." Paged " TOTAL-PAGED-HIMEM MiB CR

CELL ' MARK-CHILD ALLOCATE-TRACING CONSTANT test-alloc
test-alloc test-alloc !
test-alloc MARK-CHILD

." Free mem: " FREE-HIMEM MiB CR
." Used mem: " USED-HIMEM MiB CR

DEBUG

.( Done with init!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
