: qemu-shutdown ?( --)
  $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

1 IPB CONSTANT mb2 ?( The multiboot2 info structure's address)
: mb2-each-tag ?( xt --)
  mb2 D@ mb2 +
  mb2 8 +
  DO
    I SWAP DUP >R EXECUTE R>
    SWAP IF LEAVE THEN
    I 4 + D@ ALIGN-TO-CELL
  +LOOP DROP ;

: mmap-entry-base-addr @ ;
: mmap-entry-length 8 + @ ;
: mmap-entry-type $10 + D@ ;
: add-free-list-link
  DUP mmap-entry-type 1 <> IF RETURN THEN
  .S
  DUP ." link at 0x" . CR
  DUP ."    from 0x" mmap-entry-base-addr . CR
  DUP ."      to 0x" DUP mmap-entry-base-addr SWAP mmap-entry-length + . CR
  DUP ." with type " mmap-entry-type . CR CR
  .S DROP ;

: mmap-start-addr $10 + ;
: mmap-end-addr DUP mmap-start-addr SWAP 4 + D@ $10 - + ;
: mmap-entry-size 8 + D@ ;
: build-free-list
  ." mmap tag at 0x" DUP . CR
  >R \ Yeah, this is unidiomatic as heck.
  R@ DROP
  R@ mmap-end-addr
  R@ mmap-start-addr
  R> mmap-entry-size -ROT ?DO
    DUP I add-free-list-link
  +LOOP BP DROP ;

:NONAME DUP D@ 6 = IF build-free-list TRUE ELSE DROP FALSE THEN ; mb2-each-tag

.( Done with init.f!)
BP QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
