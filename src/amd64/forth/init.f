: qemu-shutdown ?( --)
  $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

1 IPB . INT3

.( Done with init.f!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
