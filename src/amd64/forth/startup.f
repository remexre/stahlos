: qemu-shutdown $2000 $604 OUTW ;
' qemu-shutdown IS-QUIT

\ Parsing.
\ : PARSE ( char "ccc<char>" -- c-addr u )
  \ BEGIN
    \ SOURCE-REST DUP
  \ WHILE
    \ DEBUG 2DROP
  \ REPEAT ;

\ [CHAR] ) PARSE foo bar baz ) .( end) DEBUG

.( Done with startup.f!)
\ QUIT

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
