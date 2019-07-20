\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE '
  ] PARSE-NAME 2DUP FIND-HEADER DUP (IF) [
  HERE 0 ,
  ] HEADER>CFA -ROT 2DROP EXIT [
  DOES>ENTER
HERE SWAP ! ' DROP COMPILE, $1ffff8 @ $28 + @ COMPILE,
CREATE : ] ALIGN CREATE ] EXIT [ DOES>ENTER
CREATE ;
  ' [ COMPILE,
  ] (LITERAL) EXIT COMPILE, DOES>ENTER EXIT [
  DOES>ENTER IMMEDIATE
: :NONAME CREATE-NONAME ] ;

\ Some compilation helpers. Later, ', ['], COMPILING, and POSTPONE need to be
\ redefined to ABORT on a not-found word.
: LITERAL [ ' (LITERAL) DUP COMPILE, , ] COMPILE, , ; IMMEDIATE
: ['] [ ' (LITERAL) ] LITERAL COMPILE, ' , ; IMMEDIATE
: COMPILING ['] (LITERAL) COMPILE, ' COMPILE, ['] COMPILE, COMPILE, ; IMMEDIATE
: POSTPONE ' COMPILE, ; IMMEDIATE
: LAST LATEST HEADER>CFA ;
: LITERAL-R COMPILING (LITERAL-R) , ; IMMEDIATE

\ Parenthetical and documentation comments.
: IS-CLOSE-PAREN? $29 = ;
: ( SOURCE-REST ['] IS-CLOSE-PAREN? STRING-FIND-PRED 1+ >IN +! ; IMMEDIATE
: ?(
  SOURCE-REST ['] IS-CLOSE-PAREN? STRING-FIND-PRED
  SOURCE-REST DROP 2DUP SWAP 1- SWAP C! LATEST 8 + !
  1+ >IN +! ; IMMEDIATE

\ Simple utils.
: 2SWAP ?( x y z w -- z w x y) >R -ROT R> -ROT ;
: 2OVER ?( x y z w -- x y z w x y) 2>R 2DUP 2R> 2SWAP ;
: POW2 ?( u -- u) 1 SWAP LSHIFT ;
: ALIGN-DOWN-TO-POW2 ?( u b -- u) POW2 1- INVERT AND ;
: ALIGN-UP-TO-POW2 ?( u b -- u) POW2 1- DUP ROT + SWAP INVERT AND ;
: ALIGNED ?( u -- u) 3 ALIGN-UP-TO-POW2 ;
: NIP ?( x y -- y) SWAP DROP ;
: NOT ?( flag -- flag) 0= ;
: TUCK ?( x y -- y x y) SWAP OVER ;
: RETURN ?( --) RDROP ;
: WITHIN ?( x l u -- l<=x&&x<u) OVER - >R - R> U< ;

\ Comparisons.
: <= > INVERT ;
: >= < INVERT ;
: U<= U> INVERT ;
: U>= U< INVERT ;

\ Words that are "deferred" through the process area.
: GET-ABORT PROCESS-POINTER $40 + @ ;
: GET-BP PROCESS-POINTER $48 + @ ;
: GET-EMIT PROCESS-POINTER $50 + @ ;
: GET-QUIT PROCESS-POINTER $58 + @ ;

: IS-ABORT ?( addr --) PROCESS-POINTER $40 + ! ;
: IS-BP ?( addr --) PROCESS-POINTER $48 + ! ;
: IS-EMIT ?( addr --) PROCESS-POINTER $50 + ! ;
: IS-QUIT ?( addr --) PROCESS-POINTER $58 + ! ;

: ABORT GET-ABORT EXECUTE ;
: BP ?( --) GET-BP EXECUTE ;
: EMIT ?( char --) GET-EMIT EXECUTE ;
: QUIT EMPTY-RETURN-STACK GET-QUIT EXECUTE ;

\ Relative pointer write. This will probably only ever be useful for (re)writing
\ CALL/JMP target addresses. addr should point to the byte after the 0xe8/0xe9.
: D!REL ?( val addr --) TUCK - 4 - SWAP D! ;
: D,REL ?( val --) HERE 4 ALLOT D!REL ;

\ DOES>. There might be a better way to define it than this. The inline assembly
\ in DOES> is something that ideally would be factored out...
: (DOES>) R@ CELL+ LAST 1+ D!REL ;
: DOES> COMPILING (DOES>) COMPILING EXIT ['] ((DODOES)) $e8 C, D,REL ; IMMEDIATE

\ Constants and variables.
: CONSTANT ?( n --) CREATE , DOES> @ ;
: VARIABLE ?( --) CREATE 0 , DOES> ;

\ Arrays.
: ARRAY ?( n "<spaces>name" --) CREATE CELLS ALLOT
  DOES> ?( n -- addr) SWAP CELLS + ;

\ Conditionals.
$10 ARRAY (IF-STACK)
VARIABLE (IF-IDX)
: (IF-PUSH) ?( n --) (IF-IDX) @ (IF-STACK) ! 1 (IF-IDX) +! ;
: (IF-POP) ?( -- n) -1 (IF-IDX) +! (IF-IDX) @ (IF-STACK) @ ;
: IF COMPILING (IF) HERE (IF-PUSH) 0 , ; IMMEDIATE
: ?IF COMPILING ?DUP POSTPONE IF ; IMMEDIATE
: ELSE COMPILING (JUMP) HERE 0 , HERE (IF-POP) ! (IF-PUSH) ; IMMEDIATE
: ENDIF HERE (IF-POP) ! ; IMMEDIATE
: THEN POSTPONE ENDIF ; IMMEDIATE

\ Leave.
8 ARRAY (LEAVE-STACK)
VARIABLE (LEAVE-IDX)
: (LEAVE-PUSH) ?( addr --) (LEAVE-IDX) @ (LEAVE-STACK) ! 1 (LEAVE-IDX) +! ;
: (LEAVE-POP) ?( -- addr) -1 (LEAVE-IDX) +! (LEAVE-IDX) @ (LEAVE-STACK) @ ;
: LEAVE COMPILING (JUMP) HERE (LEAVE-PUSH) 0 , ; IMMEDIATE
: (RESOLVE-LEAVES) ?( --)
  \ We're manually building a BEGIN-WHILE-REPEAT loop, since it's not yet
  \ definable.
  [ HERE ]
  (LEAVE-IDX) @ 0= IF (JUMP) [ HERE SWAP 0 , ] THEN
  HERE (LEAVE-POP) ! (JUMP) [ , HERE SWAP ! ] ;

\ Loops.
8 ARRAY (LOOP-STACK)
VARIABLE (LOOP-IDX)
: (LOOP-PUSH) ?( addr --) (LOOP-IDX) @ (LOOP-STACK) ! 1 (LOOP-IDX) +! ;
: (LOOP-POP) ?( -- addr) -1 (LOOP-IDX) +! (LOOP-IDX) @ (LOOP-STACK) @ ;
: BEGIN HERE (LOOP-PUSH) ; IMMEDIATE
: AGAIN COMPILING (JUMP) (LOOP-POP) , (RESOLVE-LEAVES) ; IMMEDIATE
: WHILE COMPILING (IF) HERE (LEAVE-PUSH) 0 , ; IMMEDIATE
: REPEAT POSTPONE AGAIN ; IMMEDIATE

: (DO) R> -ROT 2>R 3 CELLS + >R ;
: (?DO)
  R> DUP @ SWAP CELL+
  R> R>
  2DUP = IF
    3 PICK >R DROP DROP >R >R
  ELSE
    >R >R >R DROP
  THEN ;
: (+LOOP) R> R> 2 PICK + >R NIP @ >R ;

: ?DO ?( limit base --)
  COMPILING 2>R HERE COMPILING (?DO) HERE 0 , (LOOP-PUSH) (LOOP-PUSH) ; IMMEDIATE
: DO ?( limit base --) COMPILING (DO) POSTPONE ?DO ; IMMEDIATE
: +LOOP ?( n --)
  COMPILING (+LOOP)
  (LOOP-POP) , HERE (LOOP-POP) ! (RESOLVE-LEAVES)
  COMPILING RDROP COMPILING RDROP ; IMMEDIATE
: LOOP ?( --) COMPILING (LITERAL) 1 , POSTPONE +LOOP ; IMMEDIATE

: TIMES COMPILING FALSE POSTPONE ?DO ; IMMEDIATE

: I 1 RPICK ;
: J 3 RPICK ;
: K 5 RPICK ;

\ Deferring. Note: since the standard library is shared between all processes,
\ DEFER must not be used to define standard words which can vary per-process.
: DEFER CREATE COMPILING NOOP DOES> @ EXECUTE ;
: DEFER! 5 + ! ;
: DEFER@ 5 + @ ;
: IS
  GET-STATE IF
    POSTPONE ['] POSTPONE DEFER!
  ELSE
    ' DEFER!
  THEN ; IMMEDIATE

\ I/O primitives.
: EMIT-DEFAULT $e9 OUTB ;
' EMIT-DEFAULT IS-EMIT
: TYPE TIMES DUP I + C@ EMIT LOOP DROP ;
$0a CONSTANT NL
$20 CONSTANT BL
: CR    NL EMIT ;
: SPACE BL EMIT ;
: N. N>STR TYPE ;
: . N. SPACE ;
: TYPELN TYPE CR ;

: .S
  DEPTH $3c EMIT DUP N. $3e EMIT
  TIMES
  SPACE DEPTH I - 1- PICK N.
  LOOP CR ;
: D. GET-BASE SWAP DECIMAL . $10 = IF HEX ELSE DECIMAL THEN ;
: H. GET-BASE SWAP HEX     . $10 = IF HEX ELSE DECIMAL THEN ;

\ Debugging tools.
' INT3 IS-BP
: DEBUG .S BP ;

\ Output.
: IS-QUOTE? $22 = ;
: .( SOURCE-REST OVER SWAP
  ['] IS-CLOSE-PAREN? STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING TYPELN ; IMMEDIATE
: S" SOURCE-REST OVER SWAP
  ['] IS-QUOTE? STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING
  GET-STATE IF COMPILING (S") S, ENDIF
  ; IMMEDIATE
: ." POSTPONE S"
  GET-STATE IF COMPILING TYPE ELSE TYPE ENDIF
  ; IMMEDIATE

\ More utils.
: @! ?( addr xt --) SWAP DUP >R @ SWAP EXECUTE R> ! ;
: CSTR>STR ?( addr -- addr len) DUP BEGIN DUP C@ WHILE 1+ REPEAT OVER - ;
: DISCARD ?( x_k ... x_0 k --) TIMES DROP LOOP ;
: HLT-LOOP ?( --) BEGIN HLT AGAIN ;
: 0RETURN ?( u | 0 -- u) DUP 0= IF DROP RDROP THEN ;
: TODO ?( --)
  COMPILING (S")
  S" TODO: " S,
  COMPILING TYPE
  LATEST HEADER>NAME POSTPONE LITERAL
  COMPILING COUNT
  COMPILING TYPELN
  COMPILING DEBUG
  POSTPONE BEGIN
  COMPILING HLT
  POSTPONE AGAIN ; IMMEDIATE
: WORDS ?( --) LATEST BEGIN DUP WHILE DUP HEADER>NAME COUNT TYPE SPACE @ REPEAT CR ;

\ Aborting.
: ABORT-DEFAULT ?( k*n --) DEPTH DISCARD QUIT ;
' ABORT-DEFAULT IS-ABORT
: ABORT"
  COMPILING 0=
  POSTPONE IF
  POSTPONE S"
  COMPILING TYPELN
  COMPILING ABORT
  POSTPONE THEN ; IMMEDIATE

\ Value.
: VALUE ?( n --) CREATE , DOES> @ ;
: TO ?( n --)
  ' 5 + GET-STATE IF POSTPONE LITERAL COMPILING ! ELSE ! THEN ; IMMEDIATE

\ Modules.
VARIABLE MODULE-EXPORT-COUNT
VARIABLE MODULE-PREVIOUS
: MODULE 0 MODULE-EXPORT-COUNT ! LATEST MODULE-PREVIOUS ! ;
: END-MODULE() MODULE-PREVIOUS @ DICT-HEAD ! ;
: END-MODULE(
  MODULE-PREVIOUS @
  BEGIN
    PARSE-NAME 2DUP S" )" STRING= NOT
  WHILE
    FIND-HEADER 1 MODULE-EXPORT-COUNT +!
  REPEAT
  2DROP DUP DICT-HEAD !
  MODULE-EXPORT-COUNT @ 0 ?DO OVER -ROT ! LOOP DROP ;

\ Character literals.
: [CHAR] PARSE-NAME ABORT" Missing word for [CHAR]" C@ ; IMMEDIATE

\ Some math words.
: */ */MOD NIP ;
: /MOD 1 -ROT */MOD ;
: * *D NIP ;
: / /MOD NIP ;
: MOD /MOD DROP ;

\ Access to the IPB.
$1ffff8 @ CONSTANT IPB-START ?( start address of the IPB)
: IPB-ADDR ?( n -- addr) CELLS IPB-START + ;
: IPB ?( n -- u) IPB-ADDR @ ;

\ Reading documentation.
: ? ?( "<spaces>name" --)
  PARSE-NAME 2DUP DUP ABORT" Missing word for ?"
  FIND-HEADER ?IF
    8 + @ ?IF
      -ROT ." Documentation for " TYPE ." : (" COUNT TYPE ." )" CR
    ELSE
      ." No docs for word " TYPELN
    THEN
  ELSE
    ." No such word " TYPELN
  THEN ;

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

\ Allocation. There's no FREE word, since we (eventually will) garbage collect.
\ TODO: Rework this for Cheney's algorithm.
$ffff800000000000 CONSTANT MIN-PAGED-HIMEM-ADDR
$ffff800000000000 VALUE MAX-PAGED-HIMEM-ADDR
VARIABLE HIMEM-NEXT-FREE-ADDR
$ffff800000000000 HIMEM-NEXT-FREE-ADDR !
: TOTAL-PAGED-HIMEM ?( -- u) MAX-PAGED-HIMEM-ADDR MIN-PAGED-HIMEM-ADDR - ;
: FREE-HIMEM ?( -- u) MAX-PAGED-HIMEM-ADDR HIMEM-NEXT-FREE-ADDR @ - ;
: USED-HIMEM ?( -- u) HIMEM-NEXT-FREE-ADDR @ MIN-PAGED-HIMEM-ADDR - ;

DEFER MAYBE-YIELD
: ALLOCATE-TRACING ?( l xt -- addr)
  HIMEM-NEXT-FREE-ADDR @
  ROT DUP $18 + ALIGNED HIMEM-NEXT-FREE-ADDR +!
  OVER ! CELL+
  TUCK ! CELL+
  DUP 0 ! CELL+ ;
: ALLOCATE-TRACING-ZEROED ?( l xt -- addr)
  SWAP DUP ROT ALLOCATE-TRACING DUP ROT ERASE ;
: ALLOCATE ?( l -- addr) ['] DROP ALLOCATE-TRACING ;
: ALLOCATE-ZEROED ?( l -- addr) DUP ALLOCATE TUCK ERASE ;

: MARK-CHILD ?( addr --)
  DUP 0>= IF ." Found non-GC word 0x" H. ." when marking!" CR RETURN THEN
  DUP CELL- DUP @ 1 AND 0=
  IF
    1 OVER @ OR OVER ! CELL- @ EXECUTE
  ELSE
    2DROP
  THEN ;

\ Process management.
MODULE
  : gc-process-area ?( addr --) TODO DROP ;
  : gc-process-entry ?( addr --) TODO DROP ;
  : gc-run-queue-link ?( addr --) TODO DROP ;

  : allocate-process-area ?( -- addr)
    $400 ['] gc-process-area ALLOCATE-TRACING-ZEROED ;
  : allocate-process-entry ?( -- addr)
    $18 ['] gc-process-entry ALLOCATE-TRACING ;
  : allocate-run-queue-link ?( -- addr)
    $18 ['] gc-run-queue-link ALLOCATE-TRACING ;

  : link-prev ?( link-addr -- link-addr-addr) ;
  : link-next ?( link-addr -- link-addr-addr) CELL+ ;
  : link-area ?( link-addr -- area-addr-addr) CELL+ CELL+ ;

  $200000 CONSTANT process-table
  : PID>AREA ?( pid -- addr | 0)
    DUP $ffff AND CELLS process-table +
    BEGIN @ DUP WHILE 2DUP 8 + @ = IF $10 + @ LEAVE THEN REPEAT
    NIP ;

  8 IPB-ADDR CONSTANT run-queue
  9 IPB-ADDR CONSTANT interrupts

  : SETUP-SCHEDULER ?( --)
    allocate-run-queue-link
    DUP DUP link-prev !
    DUP DUP link-next !
    DUP PROCESS-POINTER SWAP link-area !
    run-queue ! ;

  : make-run-queue-link ?( area-addr prev-link -- new-link)
    SWAP allocate-run-queue-link
    TUCK link-area !
    2DUP link-prev !
    TUCK link-next SWAP link-next @ SWAP !
    DUP DUP link-next @ link-prev !
    DUP DUP link-prev @ link-next ! ;

  : do-schedule ?( area-addr -- ) run-queue ['] make-run-queue-link @! ;
  : area>flags ?( area-addr -- flags-addr) $28 + ;
  : asleep? ?( area-addr --) area>flags @ 2 TEST-FLAG ;
  : set-sleep-flag ?( flags -- flags) 2 SET-FLAG ;
  : unset-sleep-flag ?( flags -- flags) 2 UNSET-FLAG ;
  : set-asleep ?( area-addr --) area>flags ['] set-sleep-flag @! ;
  : set-awake ?( area-addr --) area>flags ['] unset-sleep-flag @! ;
  : SCHEDULE ?( pid --)
    PID>AREA 0RETURN DUP asleep? IF DUP set-awake do-schedule ELSE DROP THEN ;

  6 IPB-ADDR CONSTANT quantum-addr
  : TICK-QUANTUM ?( --) quantum-addr @ 1 SAT- quantum-addr ! ;

  : YIELD ?( --)
    run-queue @ link-next @ DUP run-queue ! link-area @ CONTEXT-SWITCH ;
  :NONAME ?( --) quantum-addr @ 0= IF YIELD THEN ; IS MAYBE-YIELD

  : remove-run-queue-link ?( link-addr -- next-addr)
    DUP link-next @ SWAP link-prev @ 2DUP
    SWAP link-prev ! SWAP DUP ROT link-next ! ;
  : remove-from-run-queue ?( --) run-queue ['] remove-run-queue-link @! ;
  : SLEEP ?( --) PROCESS-POINTER set-asleep remove-from-run-queue YIELD ;

  : remove-from-process-list ?( pid addr --)
    BEGIN
      DUP @ ABORT" remove-from-process-list called on non-existent PID"
      2DUP @ CELL+ @
      = IF NIP DUP @ @ SWAP ! LEAVE THEN
      @
    AGAIN ;
  : remove-from-process-table ?( pid --)
    DUP $ffff AND CELLS process-table + remove-from-process-list ;
  : PID ?( -- pid) PROCESS-POINTER @ ;
  : DIE ?( --) remove-from-run-queue PID remove-from-process-table
    run-queue @ link-area @ CONTEXT-SWITCH ;

  :NONAME [ DROP HERE ] R> EXECUTE QUIT ;
  CONSTANT entrypoint

  : make-process-area ?( u_k ... u_1 k xt pid -- addr)
    allocate-process-area
    TUCK ! \ Process ID
    TUCK $1f8 + ! \ XT
    OVER 0 DO
      I 2 + PICK
      OVER $3f0 + I CELLS - !
    LOOP
    $504f544b43415453 OVER $3f8 + ! \ Stack top canary
    TRUE OVER 8 + ! \ Start of Source
    4 OVER $28 + ! \ Flags; we start asleep so SCHEDULE will schedule us
    ['] GET-ABORT OVER $40 + ! \ ABORT
    ['] GET-BP    OVER $48 + ! \ BP
    ['] GET-EMIT  OVER $50 + ! \ EMIT
    ['] GET-QUIT  OVER $58 + ! \ QUIT
    entrypoint OVER $e8 + ! \ Stored RSI
    OVER CELLS $3f8 SWAP - OVER + OVER $f0 + ! \ Stored RSP
    DUP $1f8 + OVER $f8 + ! \ Stored RBP
    >R DISCARD R> ;
  : make-process-entry ?( addr pid -- addr)
    allocate-process-entry
    TUCK 8 + !
    TUCK $10 + ! ;

  : add-to-process-table-entry ?( entry addr --)
    BEGIN DUP @ WHILE @ REPEAT ! ;
  : add-to-process-table ?( pid addr --)
    OVER make-process-entry
    SWAP $ffff AND CELLS
    process-table + add-to-process-table-entry ;

  \ PIDs are always positive, just for mental convenience.
  : random-pid ?( -- pid) RAND-WORD 1 $3f LSHIFT 1- AND ;
  : make-pid ?( -- pid)
    random-pid BEGIN DUP PID>AREA WHILE DROP random-pid REPEAT ;

  : PID>NAME ?( pid -- addr len) PID>AREA DUP $60 + @ SWAP $68 + @ ;
  : SET-PROCESS-NAME ?( addr len --)
    PROCESS-POINTER $68 + ! PROCESS-POINTER $60 + ! ;

  : SPAWN ?( u_k ... u_1 k xt --)
    make-pid DUP
    >R make-process-area R>
    2DUP SWAP add-to-process-table
    SCHEDULE CONTEXT-SWITCH ;
END-MODULE( DIE PID PID>AREA PID>NAME SCHEDULE SET-PROCESS-NAME SETUP-SCHEDULER
  SLEEP SPAWN TICK-QUANTUM YIELD )

\ Message passing.
MODULE
  : RECV-MESSAGE TODO ;
  : SEND-MESSAGE TODO ;
END-MODULE()

\ Clearing the slate for spawned children.
: RESET-TO-STD [ LATEST ] LITERAL DICT-HEAD ! ;

.( Done with std.fth!)
\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
