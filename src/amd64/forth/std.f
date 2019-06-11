\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE ' ] PARSE-NAME FIND-HEADER EXIT [ DOES>ENTER
CREATE : ] CREATE ] EXIT [ DOES>ENTER
CREATE ;
  ' [ HEADER>CFA COMPILE,
  ] (LITERAL) EXIT COMPILE, DOES>ENTER EXIT [ DOES>ENTER IMMEDIATE

FALSE FALSE >NUMBER .S

\ Printing debug strings.
: IS-CLOSE-PAREN $29 = ;
: .( SOURCE-REST OVER SWAP
  ['] IS-CLOSE-PAREN STRING-FIND-PRED
  DUP >IN +! ;

DEBUG
.( foo)
DEBUG

\ Some helpers for more complex words.
\ : POSTPONE [ ' (LITERAL) HEADER>CFA ] LITERAL COMPILE, ' HEADER>CFA COMPILE, ; IMMEDIATE

\ : foo POSTPONE DEBUG ;
\ foo

: GT1 123 ;
' GT1 EXECUTE

.( Done!) DEBUG

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
