\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE ' ] PARSE-NAME FIND-HEADER HEADER>CFA EXIT [ DOES>ENTER
CREATE : ] CREATE ] EXIT [ DOES>ENTER
CREATE ;
  ' [ COMPILE,
  ] (LITERAL) EXIT COMPILE, DOES>ENTER EXIT [ DOES>ENTER IMMEDIATE
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
: IS-CLOSE-PAREN $29 = ;
: ( SOURCE-REST ['] IS-CLOSE-PAREN STRING-FIND-PRED 1+ >IN +! ; IMMEDIATE

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
: ELSE COMPILING (JUMP) HERE 0 , HERE (IF-POP) ! (IF-PUSH) ; IMMEDIATE
: ENDIF HERE (IF-POP) ! ; IMMEDIATE
: THEN POSTPONE ENDIF ; IMMEDIATE

\ Loops.
8 ARRAY (BREAK-STACK)
VARIABLE (BREAK-IDX)
: (BREAK-PUSH) ( addr -- ) (BREAK-IDX) @ (BREAK-STACK) ! 1 (BREAK-IDX) +! ;
: (BREAK-POP) ( -- addr ) -1 (BREAK-IDX) +! (BREAK-IDX) @ (BREAK-STACK) @ ;
: BREAK COMPILING (JUMP) HERE (BREAK-PUSH) 0 , ; IMMEDIATE
\ : (RESOLVE-BREAKS) ( addr -- )
\   BEGIN (BREAK-IDX) WHILE HERE (BREAK-POP) ! REPEAT ;
8 ARRAY (LOOP-STACK)
VARIABLE (LOOP-IDX)
: (LOOP-PUSH) ( addr -- ) (LOOP-IDX) @ (LOOP-STACK) ! 1 (LOOP-IDX) +! ;
: (LOOP-POP) ( -- addr ) -1 (LOOP-IDX) +! (LOOP-IDX) @ (LOOP-STACK) @ ;
: BEGIN COMPILING (LITERAL-R) HERE 0 , ;

\ Deferring, to make output hookable.
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
DEFER EMIT
:NONAME $e9 OUTB ; IS EMIT
$20 CONSTANT BL
: SPACE BL EMIT ;

\ Debugging tools.
DEFER BP
' BOCHS-BP IS BP
: DEBUG .S BP ;

: IS-QUOTE $22 = ;
: .( SOURCE-REST OVER SWAP
  ['] IS-CLOSE-PAREN STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING TYPE ; IMMEDIATE
: S" SOURCE-REST OVER SWAP
  ['] IS-QUOTE STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING
  GET-STATE IF COMPILING (S") S, ENDIF
  ; IMMEDIATE
: ." POSTPONE S"
  GET-STATE IF COMPILING TYPE ELSE TYPE ENDIF
  ; IMMEDIATE

\ Abort and quit.
DEFER QUIT
DEFER ABORT
\ : ABORT-DEFAULT ( k*n -- ) BEGIN DEPTH WHILE DROP REPEAT QUIT ;
\ ' ABORT-DEFAULT IS ABORT
: ABORT"
  POSTPONE IF
  POSTPONE S"
  COMPILING TYPELN
  COMPILING ABORT
  POSTPONE THEN ; IMMEDIATE

\ Character literals.
: [CHAR] PARSE-NAME ABORT" Missing word" C@ ; IMMEDIATE

: CALLEE ." start of callee" EMPTY-RETURN-STACK ." end of callee" ;
: TEST ." before callee" CALLEE ." after callee" DEBUG ;
\ : TEST DECIMAL BEGIN #12345 . AGAIN ;

.( TEST is ) ' TEST . CR

.( about to test) CR
TEST BP

." Done!" CR

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
