MODULE

#12 CONSTANT page-bits
page-bits POW2 CONSTANT page-size

\ This is guaranteed by the linker script.
$100000 CONSTANT kernel-start
$300000 CONSTANT kernel-end

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
  2DUP mb2-start -ROT WITHIN -ROT mb2-end 1- -ROT WITHIN OR ;
: intersects-module ?( l h -- flag)
  2DUP modules-start @ -ROT WITHIN -ROT modules-end @ 1- -ROT WITHIN OR ;
: intersects-null ?( l h -- flag)
  2DUP 0 -ROT WITHIN -ROT page-size 1- -ROT WITHIN OR ;

VARIABLE free-page-list
VARIABLE free-pages-size

: add-page-to-free-page-list ?( addr --)
  free-page-list @ OVER ! free-page-list !
  page-size free-pages-size +! ;

: add-range-to-free-page-list ?( l h --)
  SWAP DO I add-page-to-free-page-list page-size +LOOP ;

: free-list-range-big-enough SWAP - page-size >= ;

: align-to-page-bounds ?( l h --)
  SWAP page-bits ALIGN-UP-TO-POW2
  SWAP page-bits ALIGN-DOWN-TO-POW2 ;

: handle-possibly-intersecting-mmap-chunk
  align-to-page-bounds
  2DUP free-list-range-big-enough IF
    2DUP intersects-kernel IF
      >R kernel-start kernel-end R> 2SWAP
      handle-possibly-intersecting-mmap-chunk
      handle-possibly-intersecting-mmap-chunk
    ELSE
      2DUP intersects-mb2 IF
        >R mb2-start mb2-end R> 2SWAP
        handle-possibly-intersecting-mmap-chunk
        handle-possibly-intersecting-mmap-chunk
      ELSE
        2DUP intersects-module IF
          >R modules-start @ modules-end @ R> 2SWAP
          handle-possibly-intersecting-mmap-chunk
          handle-possibly-intersecting-mmap-chunk
        ELSE
          2DUP intersects-null IF
            >R 0 page-size R> 2SWAP
            handle-possibly-intersecting-mmap-chunk
            handle-possibly-intersecting-mmap-chunk
          ELSE
            add-range-to-free-page-list
          THEN
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

: make-free-page-list
  mb2-mmap-tag-addr @
  DUP DUP mmap-sizeof-entry ROT mmap-end-addr ROT mmap-start-addr
  ?DO DUP I handle-mmap-entry +LOOP DROP ;

END-MODULE( free-page-list make-free-page-list page-size )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
