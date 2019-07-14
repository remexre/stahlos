MODULE

1 IPB CONSTANT mb2-start ?( The multiboot2 info structure's address)
mb2-start 8 + CONSTANT mb2-tags ?( The address multiboot2 tags start at)
mb2-start D@ mb2-start + CONSTANT mb2-end ?( The multiboot2 info structure's end address)

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
VARIABLE mb2-mmap-tag-addr
VARIABLE mb2-rsdp-v1-tag-addr
VARIABLE mb2-rsdp-v2-tag-addr

VARIABLE modules-start
-1 modules-start !
VARIABLE modules-end

: handle-mb2-module-tag
  DUP 8 + D@ modules-start @ UMIN modules-start !
    #12 + D@ modules-end   @ UMAX modules-end   ! ;

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

: check-modules
  modules-start @ modules-end @ U<
  ABORT" No multiboot2 modules found, add some with module2!" ;

END-MODULE( mb2-start mb2-end modules-start modules-end mb2-mmap-tag-addr
  check-modules traverse-mb2 mb2-tags mb2-tag-size mb2-tag-type mb2-type-module )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
