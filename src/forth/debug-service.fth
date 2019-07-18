$0000000000000000 CONSTANT message-ping
$66c379ab0b7196ef CONSTANT message-debug-print
$ffffffffffffffff CONSTANT message-pong

(
:NONAME
  BEGIN RECV-MESSAGE
  CASE
    message-ping OF message-pong SEND-MESSAGE ENDOF
    message-debug-print OF
      DROP 0 ?DO DUP I + C@ $e9 OUTB LOOP FREE
    ENDOF
    DROP DROP
  ENDCASE
  AGAIN ; EXECUTE
)

.( Done with debug-service.fth!)
\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
