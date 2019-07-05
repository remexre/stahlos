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

\ Words that are "deferred" through the user area.
: ABORT USER-POINTER $38 + @ EXECUTE ;
: BP USER-POINTER $40 + @ EXECUTE ;
: EMIT ?( char -- ) USER-POINTER $48 + @ EXECUTE ;
: QUIT EMPTY-RETURN-STACK USER-POINTER $50 + @ EXECUTE ;

: IS-ABORT ?( addr -- ) USER-POINTER $38 + ! ;
: IS-BP ?( addr -- ) USER-POINTER $40 + ! ;
: IS-EMIT ?( addr -- ) USER-POINTER $48 + ! ;
: IS-QUIT ?( addr -- ) USER-POINTER $50 + ! ;

\ Relative pointer write. This will probably only ever be useful for (re)writing
\ CALL/JMP target addresses. addr should point to the byte after the 0xe8/0xe9.
: D!REL ?( val addr -- ) SWAP OVER - 4 - SWAP D! ;
: D,REL ?( val -- ) HERE 4 ALLOT D!REL ;

\ DOES>. There might be a better way to define it than this. The inline assembly
\ in DOES> is something that ideally would be factored out...
: (DOES>) R@ CELL+ LAST 1+ D!REL ;
: DOES> COMPILING (DOES>) COMPILING EXIT ['] ((DODOES)) $e8 C, D,REL ; IMMEDIATE

\ Constants and variables.
: CONSTANT ?( n -- ) CREATE , DOES> @ ;
: VARIABLE ?( -- ) CREATE 0 , DOES> ;

\ Arrays.
: ARRAY ?( n "<spaces>name" -- ) CREATE CELLS ALLOT
  DOES> ?( n -- addr ) SWAP CELLS + ;

\ Conditionals.
8 ARRAY (IF-STACK)
VARIABLE (IF-IDX)
: (IF-PUSH) ?( n -- ) (IF-IDX) @ (IF-STACK) ! 1 (IF-IDX) +! ;
: (IF-POP) ?( -- n ) -1 (IF-IDX) +! (IF-IDX) @ (IF-STACK) @ ;
: IF COMPILING (IF) HERE (IF-PUSH) 0 , ; IMMEDIATE
: ?IF COMPILING ?DUP POSTPONE IF ; IMMEDIATE
: ELSE COMPILING (JUMP) HERE 0 , HERE (IF-POP) ! (IF-PUSH) ; IMMEDIATE
: ENDIF HERE (IF-POP) ! ; IMMEDIATE
: THEN POSTPONE ENDIF ; IMMEDIATE

\ Break.
8 ARRAY (BREAK-STACK)
VARIABLE (BREAK-IDX)
: (BREAK-PUSH) ?( addr -- ) (BREAK-IDX) @ (BREAK-STACK) ! 1 (BREAK-IDX) +! ;
: (BREAK-POP) ?( -- addr ) -1 (BREAK-IDX) +! (BREAK-IDX) @ (BREAK-STACK) @ ;
: BREAK COMPILING (JUMP) HERE (BREAK-PUSH) 0 , ; IMMEDIATE
: (RESOLVE-BREAKS) ?( -- )
  \ We're manually building a BEGIN-WHILE-REPEAT loop, since it's not yet
  \ definable.
  [ HERE ]
  (BREAK-IDX) @ 0= IF (JUMP) [ HERE SWAP 0 , ] THEN
  HERE (BREAK-POP) ! (JUMP) [ , HERE SWAP ! ] ;

\ Loops.
8 ARRAY (LOOP-STACK)
VARIABLE (LOOP-IDX)
: (LOOP-PUSH) ?( addr -- ) (LOOP-IDX) @ (LOOP-STACK) ! 1 (LOOP-IDX) +! ;
: (LOOP-POP) ?( -- addr ) -1 (LOOP-IDX) +! (LOOP-IDX) @ (LOOP-STACK) @ ;
: BEGIN HERE (LOOP-PUSH) ; IMMEDIATE
: AGAIN COMPILING (JUMP) (LOOP-POP) , (RESOLVE-BREAKS) ; IMMEDIATE
: WHILE COMPILING (IF) HERE (BREAK-PUSH) 0 , ; IMMEDIATE
: REPEAT POSTPONE AGAIN ; IMMEDIATE

: (DO) R> -ROT 2>R 3 CELLS + >R ;
: (?DO)
  R> DUP @ SWAP CELL+
  R> R>
  2DUP = IF
    3 PICK >R DROP DROP DROP DROP
  ELSE
    >R >R >R DROP
  THEN ;
: (+LOOP) R> R> R> 3 PICK + >R >R SWAP DROP @ >R ;

: ?DO COMPILING 2>R HERE COMPILING (?DO) HERE 0 , (LOOP-PUSH) (LOOP-PUSH) ; IMMEDIATE
: DO COMPILING (DO) POSTPONE ?DO ; IMMEDIATE
: +LOOP COMPILING (+LOOP) (LOOP-POP) , HERE (LOOP-POP) ! ; IMMEDIATE
: LOOP COMPILING (LITERAL) 1 , POSTPONE +LOOP ; IMMEDIATE

: TIMES COMPILING FALSE COMPILING SWAP POSTPONE ?DO ; IMMEDIATE

: I 2 RPICK ;
: J 4 RPICK ;
: K 6 RPICK ;

\ Some stack manipulation words.
: NIP  SWAP DROP ;
: TUCK SWAP OVER ;
: DISCARD ?( x_k ... x_0 k -- ) TIMES DROP LOOP ;

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

\ Aborting.
: ABORT-DEFAULT ?( k*n -- ) BEGIN DEPTH WHILE DROP REPEAT QUIT ;
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
: IPB ?( n -- u ) CELLS $1ffff8 @ + @ ;

\ Reading documentation.
: DOCS ?( "<spaces>name" -- )
  PARSE-NAME 2DUP DUP ABORT" Missing word for DOCS"
  FIND-HEADER ?IF
    8 + @ ?IF
      -ROT ." Documentation for " TYPE ." : " COUNT TYPELN
    ELSE
      ." No docs for word " TYPELN
    THEN
  ELSE
    ." No such word " TYPELN
  THEN ;

.( Done with std.f!)

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
