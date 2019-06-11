\ The parts of the standard library that are implemented in Forth themselves.

\ Define function definition.
CREATE ' ] PARSE-NAME FIND-HEADER HEADER>CFA EXIT [ DOES>ENTER
CREATE : ] CREATE ] EXIT [ DOES>ENTER
CREATE ;
  ' [ COMPILE,
  ] (LITERAL) EXIT COMPILE, DOES>ENTER EXIT [ DOES>ENTER IMMEDIATE

\ Immediate quotation.
: ['] ' [ ' (LITERAL) ] LITERAL COMPILE, , ; IMMEDIATE

\ Printing debug strings.
: IS-CLOSE-PAREN $29 = ;
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
