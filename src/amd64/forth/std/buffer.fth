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

  : BUFFER-NEW ?( -- buffer)
  	allocate-buffer
	DUP buffer-data 0 ALLOCATE !
	DUP buffer-length 0 !
	DUP buffer-capacity 0 ! ;

  : buffer-fits? ?( buffer len --)
  	OVER buffer-length @ + SWAP buffer-capacity @ U<= ;

  : BUFFER-APPEND ?( buffer addr len --)
  	2 PICK OVER buffer-fits? TODO ;

  : BUFFER-LENGTH ?( buffer -- len) buffer-length @ ;
  : BUFFER>STR ?( buffer -- addr len) DUP buffer-data @ SWAP BUFFER-LENGTH ;
END-MODULE( BUFFER-NEW BUFFER-APPEND BUFFER-LENGTH BUFFER>STR )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
