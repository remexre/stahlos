1 IPB CONSTANT mb2 ?( The multiboot2 info structure's address)
mb2 8 + CONSTANT mb2-tags ?( The address multiboot2 tags start at)
mb2 D@ mb2 + CONSTANT mb2-end ?( The multiboot2 info structure's end address)

: mb2-tag-size 4 + D@ ALIGNED ;
: mb2-tag-type D@ ;

1 CONSTANT mb2-type-command-line
2 CONSTANT mb2-type-bootloader-name
3 CONSTANT mb2-type-module
6 CONSTANT mb2-type-mmap
#14 CONSTANT mb2-type-rsdp-v1
#15 CONSTANT mb2-type-rsdp-v2

VARIABLE mb2-command-line-tag-addr
VARIABLE mb2-bootloader-name-tag-addr
VARIABLE mb2-module-tag-addr
VARIABLE mb2-mmap-tag-addr
VARIABLE mb2-rsdp-v1-tag-addr
VARIABLE mb2-rsdp-v2-tag-addr

: handle-mb2-module-tag
  mb2-module-tag-addr @ IF
    ." Warning: Only the first Multiboot2 module will be loaded" CR
  ELSE
    mb2-module-tag-addr !
  THEN ;

: handle-mb2-tag
  DUP mb2-tag-type
  DUP mb2-type-command-line = IF DROP mb2-command-line-tag-addr ! ELSE
  DUP mb2-type-bootloader-name = IF DROP mb2-bootloader-name-tag-addr ! ELSE
  DUP mb2-type-module = IF DROP handle-mb2-module-tag ELSE
  DUP mb2-type-mmap = IF DROP mb2-mmap-tag-addr ! ELSE
  DUP mb2-type-rsdp-v1 = IF DROP mb2-rsdp-v1-tag-addr ! ELSE
  DUP mb2-type-rsdp-v2 = IF DROP mb2-rsdp-v2-tag-addr ! ELSE
  2DROP THEN THEN THEN THEN THEN THEN ;

: traverse-mb2 mb2-end mb2-tags ?DO I handle-mb2-tag I mb2-tag-size +LOOP ;

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
