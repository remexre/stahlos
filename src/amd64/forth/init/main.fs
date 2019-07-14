MODULE
  : qemu-shutdown ?( --) $2000 $604 OUTW BEGIN HLT AGAIN ;
  ' qemu-shutdown IS-QUIT
END-MODULE()

traverse-mb2

\ Check that (any) modules were defined.
modules-start @ modules-end @ U<
  ABORT" No multiboot2 modules found, add some with module2!" ;

make-free-page-list
setup-himem
spawn-mb2-modules

.( Done with init!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
