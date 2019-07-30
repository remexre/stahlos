MODULE
  \ Layout:
  \ 
  \  $0 +----------+
  \     |   Data   |
  \  $8 +----------+
  \     |  Length  |
  \ $10 +----------+
  \     | Capacity |
  \ $18 +----------+
  : buffer-data ?( addr -- addr) ;
  : buffer-length ?( addr -- addr) CELL+ ;
  : buffer-capacity ?( addr -- addr) CELL+ CELL+ ;

  : gc-buffer ?( addr --) TODO DROP ;
  : allocate-buffer ?( -- addr) $18 ['] gc-buffer ALLOCATE-TRACING-ZEROED ;

  : buffer-new-sized ?( len -- buffer)
    allocate-buffer
    OVER ALLOCATE
    OVER buffer-data !
    2DUP buffer-length !
    buffer-capacity ! ;
  : BUFFER-NEW ?( -- buffer) 0 buffer-new-sized ;

  : buffer-fits? ?( buffer len -- flag)
    OVER buffer-length @ + SWAP buffer-capacity @ U<= ;
  : buffer-or-grow ?( buffer len -- buffer)
    2DUP buffer-fits? IF DROP ELSE
      OVER buffer-capacity @ +
      buffer-new-sized
      OVER buffer-data @
      OVER buffer-data @
      3 PICK buffer-length @
      MOVE NIP
    THEN ;

  : BUFFER-APPEND ?( buffer addr len --)
    2 PICK OVER buffer-fits? TODO ;

  : BUFFER-LENGTH ?( buffer -- len) buffer-length @ ;
  : BUFFER>STR ?( buffer -- addr len) DUP buffer-data @ SWAP BUFFER-LENGTH ;
END-MODULE( BUFFER-NEW BUFFER-APPEND BUFFER-LENGTH BUFFER>STR )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
