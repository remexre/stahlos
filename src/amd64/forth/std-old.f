\ The parts of the standard library that are implemented in Forth themselves.

\ Some control-flow words.
: 2DROP ( X1 X2 --             ) DROP DROP      ;
: 2DUP  ( X1 X2 -- X1 X2 X1 X2 ) OVER OVER      ;
: 2R>   ( -- X1 X2 ) ( R: X1 X2 -- ) R> R> R> ROT >R SWAP     ;
: 2>R   ( X1 X2 -- ) ( R: -- X1 X2 ) R> ROT ROT SWAP >R >R >R ;


: CHAR WORD DROP c@ ;
: BL $20 ; \ TODO $20 CONSTANT BL
: QUOTE $22 EMIT ;
: SPACE BL EMIT ;
: SPACES TIMES SPACE LOOP ;

HERE 0 , : ASCII.BUF LITERAL ; \ TODO use a VARIABLE
: ASCII. ( X -- ) ASCII.BUF ! ASCII.BUF 4 TYPE ;

\ TODO This is slower than it should be -- is it worth it to do bit-fuckery
\ to optimize the division?
: CURSOR-AFTER-CR ( -- FLAG ) [ $123456 #12 + @ ] LITERAL W@ #80 MOD 0= ;
: . ( X -- ) CURSOR-AFTER-CR UNLESS SPACE ENDIF .NOSPACE ;

: ." COMPILE S" STATE @ IF [COMPILE] TYPE ELSE TYPE ENDIF ; IMMEDIATE

\ Base-setting words.
: BINARY   #2 BASE ! ;
: DECIMAL #10 BASE ! ;
: HEX     #16 BASE ! ;

\ Some more helpers.
: CRR CR REFRESH ;
: HALT HLT RECURSE ;
: REBOOT ." Rebooting, please hold..." CRR $fe $64 OUTB HALT ;
: TODO .S INT3 HLT RECURSE ;

\ Define the REPL.
: QUIT
  \ Empty the return stack.
  [ $123456 #28 + ] LITERAL @ UNSAFE-SET-RETURN-STACK-PTR
  \ Go into interpret mode.
  0 STATE !
  \ Set the error handler to call quit again. (It doesn't actually get set
  \ until later.)
  COMPILE [LITERAL] [ HERE 0 , ] SET-ERROR-HANDLER
  \ Interpret a line.
  READ-LINE INTERPRET
  \ If we're not after a newline, add one.
  CURSOR-AFTER-CR UNLESS CR ENDIF
  \ If we're ok, print the "ok" message.
  INTERPRET-OK IF ." ok" CRR ENDIF
  \ Loop.
  RECURSE ;
\ This is the error handler we use.
:NONAME GET-ERROR-MESSAGE TYPE CRR QUIT ;
\ We set it in the above definition directly.
LATEST CFA SWAP !

\ We also define ABORT, which heavily leans on QUIT.
: ABORT [ $123456 #24 + ] LITERAL @ UNSAFE-SET-PARAM-STACK-PTR QUIT ;

HEX

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
