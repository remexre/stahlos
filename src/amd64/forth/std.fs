\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE '
  ] PARSE-NAME 2DUP FIND-HEADER DUP (IF) [
  HERE 0 ,
  ] HEADER>CFA -ROT 2DROP EXIT [
  DOES>ENTER
HERE SWAP ! ' DROP COMPILE, $1ffff8 @ $28 + @ COMPILE,
CREATE : ] CREATE ] EXIT [ DOES>ENTER
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
: ALIGN ?( u b -- u) POW2 1- DUP ROT + SWAP INVERT AND ;
: ALIGN-DOWN-TO-POW2 ?( u b -- u) POW2 1- INVERT AND ;
: ALIGN-UP-TO-POW2 ?( u b -- u) POW2 1- DUP ROT + SWAP INVERT AND ;
: ALIGNED ?( u -- u) 3 ALIGN-UP-TO-POW2 ;
: ALIGN-HERE-TO-POW2 ?( b --) HERE SWAP ALIGN-UP-TO-POW2 HERE - ALLOT ;
: NIP ?( x y -- y) SWAP DROP ;
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
\ DEFER must not be used here, since the definition will be global.
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
:NONAME $e9 OUTB ; IS-EMIT
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
: CSTR>STR ?( addr -- addr len) DUP BEGIN DUP C@ WHILE 1+ REPEAT OVER - ;
: DISCARD ?( x_k ... x_0 k --) TIMES DROP LOOP ;
: TODO ?( --)
  COMPILING (S")
  S" TODO: " S,
  COMPILING TYPE
  LATEST HEADER>NAME POSTPONE LITERAL
  COMPILING COUNT
  COMPILING TYPELN
  COMPILING DEBUG ; IMMEDIATE
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

\ Allocation. There's no FREE word, since we (eventually will) garbage collect.
$ffff800000000000 CONSTANT MIN-PAGED-HIMEM-ADDR
$ffff800000000000 CONSTANT MAX-PAGED-HIMEM-ADDR
VARIABLE HIMEM-NEXT-FREE-ADDR
$ffff800000000000 HIMEM-NEXT-FREE-ADDR !
: TOTAL-PAGED-HIMEM ?( -- u) MAX-PAGED-HIMEM-ADDR MIN-PAGED-HIMEM-ADDR - ;
: FREE-HIMEM ?( -- u) MAX-PAGED-HIMEM-ADDR HIMEM-NEXT-FREE-ADDR @ - ;
: USED-HIMEM ?( -- u) HIMEM-NEXT-FREE-ADDR @ MIN-PAGED-HIMEM-ADDR - ;

: ALLOCATE-TRACING ?( l xt -- addr)
  HIMEM-NEXT-FREE-ADDR @
  ROT DUP #24 + ALIGNED HIMEM-NEXT-FREE-ADDR +!
  OVER ! CELL+
  DUP -ROT ! CELL+
  DUP 0 ! CELL+ ;
: ALLOCATE ?( l -- addr) ['] DROP ALLOCATE-TRACING ;

: MARK-CHILD ?( addr --)
  DUP 0>= IF ." Found non-GC word 0x" H. ." when marking!" CR RETURN THEN
  DUP CELL- DUP @ 1 AND 0=
  IF
    1 OVER @ OR OVER ! CELL- @ EXECUTE
  ELSE
    2DROP
  THEN ;

\ Context switching.
: PROCESS-POINTER-FOR ?( n -- addr) CELLS $202000 + ;
: PID ?( -- n) PROCESS-POINTER @ ;

CREATE (STD-MARKER)
.( Done with std.fs!)

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
