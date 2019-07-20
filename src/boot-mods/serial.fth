S" serial" SET-PROCESS-NAME

$3f8 VALUE port
: int-enable port 1 + ;
: int-ident port 2 + ;
: line-ctrl port 3 + ;
: modem-ctrl port 4 + ;
: line-status port 5 + ;
: modem-status port 6 + ;
: scratch port 7 + ;
: dlab-lsb port ;
: dlab-msb port 1 + ;

: setup-port-from-args ?( addr len --)
  DUP IF HEX >NUMBER ABORT" Invalid port!" TO port THEN ;

: init-serial ?( --)
  $00 int-enable OUTB \ TODO Make this interrupt-driven...
  $80 line-ctrl OUTB \ Enable DLAB
  $0c dlab-lsb OUTB \ 9600 baud
  $00 dlab-msb OUTB \ 9600 baud
  $03 line-ctrl OUTB \ Disable DLAB, set 8N1
  $c7 int-ident OUTB \ TODO Figure out what this does...
  $0b modem-status OUTB ; \ TODO Figure out what this does...

: read-ready? ?( -- flag) line-status INB 0 TEST-FLAG ;
: write-ready? ?( -- flag) line-status INB 5 TEST-FLAG ;

: read-byte ?( -- byte) BEGIN read-ready? NOT WHILE YIELD REPEAT port INB ;
: write-byte ?( byte --) BEGIN write-ready? NOT WHILE YIELD REPEAT port OUTB ;

: main
  ." Entering serial.main..." CR
  BEGIN
    YIELD
    ." serial.main loop" CR
  AGAIN ; 

setup-port-from-args
init-serial
' write-byte IS-EMIT
.( serial port inited!)
main

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
