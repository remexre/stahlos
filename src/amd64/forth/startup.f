: qemu-shutdown $2000 $604 OUTW BEGIN HLT AGAIN ;
' qemu-shutdown IS-QUIT

\ Parsing.
\ : PARSE ( char "ccc<char>" -- c-addr u )
  \ BEGIN
    \ SOURCE-REST DUP
  \ WHILE
    \ DEBUG 2DROP
  \ REPEAT ;

\ [CHAR] ) PARSE foo bar baz ) .( end) DEBUG

: TEST 0 5
  DO
  I .S DROP
  LOOP
  ." Last part of TEST!" CR ;

." about to TEST" CR
TEST
." wow, TEST finished!" CR

.( Done with startup.f!)
QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
