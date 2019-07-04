\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE '
  ] PARSE-NAME 2DUP FIND-HEADER DUP (IF) [
  HERE 0 ,
  ] HEADER>CFA -ROT 2DROP EXIT [
  DOES>ENTER
HERE SWAP ! ' DROP COMPILE, ' .S COMPILE, ' TYPELN COMPILE, $1ffff8 @ $28 + @ COMPILE,
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

\ Parenthetical comments.
: IS-CLOSE-PAREN? $29 = ;
: ( SOURCE-REST ['] IS-CLOSE-PAREN? STRING-FIND-PRED 1+ >IN +! ; IMMEDIATE

\ Words that are "deferred" through the user area.
: ABORT USER-POINTER $38 + @ EXECUTE ;
: BP USER-POINTER $40 + @ EXECUTE ;
: EMIT USER-POINTER $48 + @ EXECUTE ;
: QUIT EMPTY-RETURN-STACK USER-POINTER $50 + @ EXECUTE ;

: IS-ABORT ( addr -- ) USER-POINTER $38 + ! ;
: IS-BP ( addr -- ) USER-POINTER $40 + ! ;
: IS-EMIT ( addr -- ) USER-POINTER $48 + ! ;
: IS-QUIT ( addr -- ) USER-POINTER $50 + ! ;

\ Relative pointer write. This will probably only ever be useful for (re)writing
\ CALL/JMP target addresses. addr should point to the byte after the 0xe8/0xe9.
: D!REL ( val addr -- ) SWAP OVER - 4 - SWAP D! ;
: D,REL ( val -- ) HERE 4 ALLOT D!REL ;

\ DOES>. There might be a better way to define it than this. The inline assembly
\ in DOES> is something that ideally would be factored out...
: (DOES>) R@ CELL+ LAST 1+ D!REL ;
: DOES> COMPILING (DOES>) COMPILING EXIT ['] ((DODOES)) $e8 C, D,REL ; IMMEDIATE

\ Constants and variables.
: CONSTANT ( n -- ) CREATE , DOES> @ ;
: VARIABLE ( -- ) CREATE 0 , DOES> ;

\ Arrays.
: ARRAY ( n "<spaces>name" -- ) CREATE CELLS ALLOT
  DOES> ( n -- addr ) SWAP CELLS + ;

\ Conditionals.
8 ARRAY (IF-STACK)
VARIABLE (IF-IDX)
: (IF-PUSH) ( n -- ) (IF-IDX) @ (IF-STACK) ! 1 (IF-IDX) +! ;
: (IF-POP) ( -- n ) -1 (IF-IDX) +! (IF-IDX) @ (IF-STACK) @ ;
: IF COMPILING (IF) HERE (IF-PUSH) 0 , ; IMMEDIATE
: ?IF COMPILING ?DUP POSTPONE IF ; IMMEDIATE
: ELSE COMPILING (JUMP) HERE 0 , HERE (IF-POP) ! (IF-PUSH) ; IMMEDIATE
: ENDIF HERE (IF-POP) ! ; IMMEDIATE
: THEN POSTPONE ENDIF ; IMMEDIATE

\ Break.
8 ARRAY (BREAK-STACK)
VARIABLE (BREAK-IDX)
: (BREAK-PUSH) ( addr -- ) (BREAK-IDX) @ (BREAK-STACK) ! 1 (BREAK-IDX) +! ;
: (BREAK-POP) ( -- addr ) -1 (BREAK-IDX) +! (BREAK-IDX) @ (BREAK-STACK) @ ;
: BREAK COMPILING (JUMP) HERE (BREAK-PUSH) 0 , ; IMMEDIATE
: (RESOLVE-BREAKS) ( -- )
  \ We're manually building a BEGIN-WHILE-REPEAT loop, since it's not yet
  \ definable.
  [ HERE ]
  (BREAK-IDX) @ 0= IF (JUMP) [ HERE SWAP 0 , ] THEN
  HERE (BREAK-POP) ! (JUMP) [ , HERE SWAP ! ] ;

\ Loops.
8 ARRAY (LOOP-STACK)
VARIABLE (LOOP-IDX)
: (LOOP-PUSH) ( addr -- ) (LOOP-IDX) @ (LOOP-STACK) ! 1 (LOOP-IDX) +! ;
: (LOOP-POP) ( -- addr ) -1 (LOOP-IDX) +! (LOOP-IDX) @ (LOOP-STACK) @ ;
: BEGIN HERE (LOOP-PUSH) ; IMMEDIATE
: AGAIN COMPILING (JUMP) (LOOP-POP) , (RESOLVE-BREAKS) ; IMMEDIATE
: WHILE COMPILING (IF) HERE (BREAK-PUSH) 0 , ; IMMEDIATE
: REPEAT POSTPONE AGAIN ; IMMEDIATE
: (?DO) R> DUP CELL+ >R
  \ 1 RPICK 2 RPICK =
  .S BP DROP ;
: (+LOOP) .S BP ABORT ;
: ?DO COMPILING 2>R HERE (LOOP-PUSH) COMPILING (?DO) HERE 0 , (LOOP-PUSH) ; IMMEDIATE
: +LOOP COMPILING (+LOOP) (LOOP-POP) , HERE (LOOP-POP) ! ; IMMEDIATE
: TIMES COMPILING FALSE COMPILING ?DO ;
: DO ( TODO ) ABORT ;
: LOOP ( TODO ) ABORT ;

\ Some stack manipulation words.
: NIP     ( X1 X2 -- X2          ) SWAP DROP ;
: TUCK    ( X1 X2 -- X2 X1 X2    ) SWAP OVER ;
\ : DISCARD ( XU ... X0 U ) TIMES DROP LOOP ;

\ Deferring. Note: since the standard library is shared between all processes,
\ DEFER must not be used, since the definition will be global.
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
$20 CONSTANT BL
: SPACE BL EMIT ;

\ Debugging tools.
' BOCHS-BP IS-BP
: DEBUG .S BP ;

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
: ABORT-DEFAULT ( k*n -- ) BEGIN DEPTH WHILE DROP REPEAT QUIT ;
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
: / /MOD NIP ;
: MOD /MOD DROP ;

\ Access to the IPB.
: IPB ( n -- u ) CELLS $1ffff8 @ + @ ;

.( Done with std.f!)

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
