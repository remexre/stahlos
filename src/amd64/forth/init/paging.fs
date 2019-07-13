VARIABLE p4-index
VARIABLE p3-index
VARIABLE p2-index
VARIABLE p1-index

\ Used as a cheap continuation. (Really, it's more like setjmp/longjmp...)
VARIABLE out-of-pages-depth
VARIABLE out-of-pages-rdepth

: out-of-pages ?( --)
  ." No free page when one was expected!" CR
  ." Bailing out of page-pages-to-himem..." CR
  BEGIN DEPTH out-of-pages-depth @ <> WHILE DROP REPEAT
  BEGIN RDEPTH out-of-pages-rdepth @ <> WHILE RDROP REPEAT ;

1 #12 LSHIFT 1- INVERT CONSTANT page-table-mask
: pte>addr page-table-mask AND ;

: p4 ?( n -- addr) CELLS 4 IPB + ;
: p4-entry ?( -- addr) p4-index @ p4 ;
: p3 ?( n -- addr) CELLS p4-entry @ pte>addr + ;
: p3-entry ?( -- addr) p3-index @ p3 ;
: p2 ?( n -- addr) CELLS p3-entry @ pte>addr + ;
: p2-entry ?( -- addr) p2-index @ p2 ;
: p1 ?( n -- addr) CELLS p2-entry @ pte>addr + ;
: p1-entry ?( -- addr) p1-index @ p1 ;

: next-free-page ?( -- addr)
  free-page-list @ DUP IF DUP @ free-page-list ! THEN ;
: must-next-free-page ?( -- addr)
  next-free-page DUP 0= IF out-of-pages THEN ;
: must-get-zeroed-page ?( -- addr) must-next-free-page DUP page-size ERASE ;

: next-p4-entry ?( -- addr)
  1 p4-index +! p4-index @ $200 < ABORT" Out of address space!?!" p4-entry ;

: new-p3 ?( --) must-get-zeroed-page 3 OR next-p4-entry ! 0 p3-index ! ;
: next-p3-entry ?( -- addr)
  1 p3-index +! p3-index @ $200 >= IF new-p3 THEN p3-entry ;

: new-p2 ?( --) must-get-zeroed-page 3 OR next-p3-entry ! 0 p2-index ! ;
: next-p2-entry ?( -- addr)
  1 p2-index +! p2-index @ $200 >= IF new-p2 THEN p2-entry ;

: new-p1 ?( --) must-get-zeroed-page 3 OR next-p2-entry ! 0 p1-index ! ;
: next-p1-entry ?( -- addr)
  1 p1-index +! p1-index @ $200 >= IF new-p1 THEN p1-entry ;

: page-page-to-himem ?( addr --) DROP ;

: addr>pte ?( addr -- u) 3 OR ;

: page-pages-to-himem ?( --) 
  $ff p4-index !
  $200 p3-index !
  $200 p2-index !
  $200 p1-index !

  DEPTH out-of-pages-depth !
  RDEPTH out-of-pages-rdepth !

  BEGIN
    next-free-page DUP
  WHILE
    DUP page-size ERASE
    addr>pte next-p1-entry !
    page-size MAX-PAGED-HIMEM-ADDR +!
  REPEAT ;

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
