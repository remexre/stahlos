$0000000000000000 constant message-ping
$66c379ab0b7196ef constant message-debug-print
$ffffffffffffffff constant message-pong

: debug-service-main
  begin get-next-message
  case
    message-ping of message-pong send-message endof
    message-debug-print of
      drop 0 ?do dup i + c@ $e9 outb loop free bochs-bp
    endof
    drop drop free
  endcase
  again ;

: spawn-debug-service ['] debug-service-main spawn ;

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
