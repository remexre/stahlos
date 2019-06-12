\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE ' ] PARSE-NAME FIND-HEADER HEADER>CFA EXIT [ DOES>ENTER
CREATE : ] CREATE ] EXIT [ DOES>ENTER
CREATE ;
  ' [ COMPILE,
  ] (LITERAL) EXIT COMPILE, DOES>ENTER EXIT [ DOES>ENTER IMMEDIATE

\ Immediate quotation.
: ['] ' [ ' (LITERAL) ] LITERAL COMPILE, , ; IMMEDIATE

\ Parenthetical comments.
: IS-CLOSE-PAREN $29 = ;
: ( SOURCE-REST ['] IS-CLOSE-PAREN STRING-FIND-PRED 1+ >IN +! ; IMMEDIATE

\ Arrays and software stacks.
: ARRAY ( n "<spaces>name" -- ) CREATE CELLS ALLOT
  DOES> ( n -- addr ) SWAP CELLS + ;
: STACK ( n "<spaces>name" -- ) CREATE 0 , DUP , CELLS ALLOT ;
: PUSH ( n addr -- ) 

\ Conditionals.
8 STACK (IF-STACK)
: IF ['] (IF) COMPILE, HERE ; IMMEDIATE
: ENDIF ( TODO ) ;
: THEN POSTPONE ENDIF ; IMMEDIATE

\ Deferring, to make output hookable.
: DEFER CREATE ['] NOOP COMPILE, DOES> @ EXECUTE ;
: DEFER! CFA>PFA ! ;
: DEFER@ CFA>PFA @ ;
: IS GET-STATE IF
  ELSE
  ENDIF ;

\ Printing debug strings.
: IS-CLOSE-QUOTE $22 = ;
: .( SOURCE-REST OVER SWAP
  ['] IS-CLOSE-PAREN STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING TYPE ;
: ." SOURCE-REST OVER SWAP
  ['] IS-CLOSE-QUOTE STRING-FIND-PRED
  DUP 1+ >IN +! 1 /STRING TYPE ;

.( foo) CR

." Done!" CR

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
