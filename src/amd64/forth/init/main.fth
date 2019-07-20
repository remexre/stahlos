traverse-mb2
check-modules
make-free-page-list
setup-himem
SETUP-SCHEDULER
S" init" SET-PROCESS-NAME
spawn-mb2-modules

FALSE CONSTANT ping
TRUE CONSTANT pong
S" Directory" FNV1A CONSTANT directory
S" Register"  FNV1A CONSTANT register

: main
  ." Entering init.main..." CR
  BEGIN
    YIELD
    ." init.main loop" CR
    INT3
  AGAIN ; 
main

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
