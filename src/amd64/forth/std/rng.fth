\ CSPRNG.
MODULE
  HERE 4 ALIGN-UP-TO-POW2 HERE - ALLOT \ Align to 16 bytes
  HERE $40 ALLOT
  HERE $40 ALLOT
  CONSTANT buffer
  CONSTANT chacha-block
  VARIABLE buffer-index
  VARIABLE entropy-index

  chacha-block $30 + CONSTANT counter

  VARIABLE word-buf
  : entropy-addr chacha-block $10 + entropy-index @ + ;
  : incr-entropy 1 entropy-index +! ;
  : ADD-ENTROPY-BYTE ?( c --) entropy-addr C@ XOR entropy-addr C! incr-entropy ;
  : ADD-ENTROPY ?( addr len --) OVER + SWAP ?DO I C@ ADD-ENTROPY-BYTE LOOP ;
  : ADD-ENTROPY-WORD ?( u --)
    word-buf ! word-buf CELL ADD-ENTROPY ;

  : TRY-ADD-RDSEED-ENTROPY ?( --)
    TRY-RDSEED IF ADD-ENTROPY-WORD ELSE DROP THEN ;

  : incr-counter ?( --) counter D@ 1+ counter D! ;
  : refill-buffer ?( --) incr-counter chacha-block buffer CHACHA20 ;

  : buffer-addr buffer buffer-index @ + ;
  : incr-buffer
    buffer-index @ DUP $40 >=
    IF refill-buffer DROP 0 ELSE 1+ THEN buffer-index ! ;
  : RAND-BYTE ?( c-addr --) buffer-addr C@ SWAP C! incr-buffer ;
  : RAND ?( addr len --) OVER + SWAP ?DO I RAND-BYTE LOOP ;
  : RAND-WORD ?( -- u) word-buf CELL RAND word-buf @ ;

  \ Read the time from CMOS.
  : cmos-read $70 OUTB $71 INB ;
  : current-time
    $32 cmos-read
    $09 cmos-read $08 LSHIFT OR
    $08 cmos-read $10 LSHIFT OR
    $07 cmos-read $18 LSHIFT OR
    $04 cmos-read $20 LSHIFT OR
    $02 cmos-read $28 LSHIFT OR
    $00 cmos-read $30 LSHIFT OR
    $06 cmos-read $38 LSHIFT OR ;

  : spin #1000000 0 DO LOOP ;

  : init-csprng
    \ Initialize chacha-block.
    $3320646e61707865 chacha-block !
    $6b20657479622d32 chacha-block 8 + !
    0 counter D!
    current-time chacha-block $38 + ! \ nonce

    \ TODO Find more sources of early-boot entropy...

    \ We wait one second of busy-looping to add more entropy to the TSC.
    current-time BEGIN DUP current-time <> WHILE spin REPEAT DROP
    RDTSC ADD-ENTROPY-WORD

    \ Add RDSEED entropy.
    TRY-ADD-RDSEED-ENTROPY
    TRY-ADD-RDSEED-ENTROPY
    TRY-ADD-RDSEED-ENTROPY
    TRY-ADD-RDSEED-ENTROPY

    \ Make the first block.
    chacha-block buffer CHACHA20 ;

  init-csprng
END-MODULE( ADD-ENTROPY ADD-ENTROPY-WORD RAND RAND-WORD )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
