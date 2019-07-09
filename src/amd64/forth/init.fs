: qemu-shutdown ?( --)
  $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

\ This is guaranteed by the linker script.
$100000 CONSTANT kernel-start
$300000 CONSTANT kernel-end

1 IPB CONSTANT mb2 ?( The multiboot2 info structure's address)
mb2 8 + CONSTANT mb2-tags ?( The address multiboot2 tags start at)
mb2 D@ mb2 + CONSTANT mb2-end ?( The multiboot2 info structure's end address)

$1000 CONSTANT page-size

6 CONSTANT mb2-type-mmap
: mb2-tag-size 4 + D@ ALIGN-TO-CELL ;
: mb2-tag-type D@ ;

1 CONSTANT mmap-entry-type-available
: mmap-entry-base-addr @ ;
: mmap-entry-length 8 + @ ;
: mmap-entry-type $10 + D@ ;
: mmap-entry-end-addr DUP mmap-entry-base-addr SWAP mmap-entry-length + ;
: mmap-start-addr $10 + ;
: mmap-end-addr DUP mmap-start-addr SWAP 4 + D@ $10 - + ;
: mmap-sizeof-entry 8 + D@ ;

: intersects-kernel ?( l h -- flag)
  2DUP kernel-start -ROT WITHIN -ROT kernel-end 1- -ROT WITHIN OR ;
: intersects-mb2 ?( l h -- flag)
  2DUP mb2 -ROT WITHIN -ROT mb2-end 1- -ROT WITHIN OR ;
: intersects-null ?( l h -- flag)
  2DUP 0 -ROT WITHIN -ROT page-size 1- -ROT WITHIN OR ;

: make-fake-block ?( addr --)
         0 OVER ! \ length
  CELL+ -1 OVER ! \ next link
  CELL+ $00000001454b4146 OVER ! \ flags+rsvd
  CELL+ 0 SWAP ! ; \ length

: make-free-list-link ?( l h --)
  2DUP SWAP - 4 CELLS - \ compute length
  DUP 2 PICK CELL- ! 2 PICK ! \ length fields
  2 IPB 2 PICK CELL+ ! \ next link
  2 CELLS - $45455246 SWAP ! \ flags+rsvd
  2 IPB-ADDR ! ;

: add-block-to-free-list ?( l h --)
  $20 - 2DUP make-fake-block make-fake-block
  SWAP $20 + SWAP make-free-list-link ;

: free-list-range-big-enough SWAP - $60 > ;
\ We need more than 0x60 space, i.e. 3 allocations worth -- we sandwich the
\ free list between two zero-length fake "used" allocations, to simplify
\ expanding-and-merging-block logic.

: handle-possibly-intersecting-mmap-chunk
  2DUP free-list-range-big-enough IF
    2DUP intersects-kernel IF
      >R kernel-start kernel-end R> 2SWAP
      handle-possibly-intersecting-mmap-chunk
      handle-possibly-intersecting-mmap-chunk
    ELSE
      2DUP intersects-mb2 IF
        >R mb2 mb2-end R> 2SWAP
        handle-possibly-intersecting-mmap-chunk
        handle-possibly-intersecting-mmap-chunk
      ELSE
        2DUP intersects-null IF
          >R 0 page-size R> 2SWAP
          handle-possibly-intersecting-mmap-chunk
          handle-possibly-intersecting-mmap-chunk
        ELSE
          add-block-to-free-list
        THEN
      THEN
    THEN
  ELSE
    2DROP
  THEN ;

: handle-mmap-available-entry 
  DUP mmap-entry-base-addr SWAP mmap-entry-end-addr
  handle-possibly-intersecting-mmap-chunk ;

: handle-mmap-entry DUP mmap-entry-type
  mmap-entry-type-available = IF handle-mmap-available-entry ELSE DROP THEN ;

: handle-mb2-mmap
  DUP DUP mmap-sizeof-entry ROT mmap-end-addr ROT mmap-start-addr
  ?DO DUP I handle-mmap-entry +LOOP DROP
  ." Allocator initialized; " FREE-BYTES D. ." bytes free" CR ;

: handle-mb2-tag DUP mb2-tag-type
  mb2-type-mmap = IF handle-mb2-mmap ELSE DROP THEN ;

: traverse-mb2 mb2-end mb2-tags ?DO I handle-mb2-tag I mb2-tag-size +LOOP ;

traverse-mb2

.( Done with init.fs!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
