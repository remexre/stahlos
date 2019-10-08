\ Process management.
MODULE
  : gc-process-area ?( addr --) TODO DROP ;
  : gc-process-entry ?( addr --) TODO DROP ;
  : gc-run-queue-link ?( addr --) TODO DROP ;

  : allocate-process-area ?( -- addr)
    $400 ['] gc-process-area ALLOCATE-TRACING-ZEROED ;
  : allocate-process-entry ?( -- addr)
    $18 ['] gc-process-entry ALLOCATE-TRACING ;
  : allocate-run-queue-link ?( -- addr)
    $18 ['] gc-run-queue-link ALLOCATE-TRACING ;

  : link-prev ?( link-addr -- link-addr-addr) ;
  : link-next ?( link-addr -- link-addr-addr) CELL+ ;
  : link-area ?( link-addr -- area-addr-addr) CELL+ CELL+ ;

  $200000 CONSTANT process-table
  : PID>AREA ?( pid -- addr | 0)
    DUP $ffff AND CELLS process-table +
    BEGIN @ DUP WHILE 2DUP 8 + @ = IF $10 + @ LEAVE THEN REPEAT
    NIP ;

  8 IPB-ADDR CONSTANT run-queue
  9 IPB-ADDR CONSTANT interrupts

  : SETUP-SCHEDULER ?( --)
    allocate-run-queue-link
    DUP DUP link-prev !
    DUP DUP link-next !
    DUP PROCESS-POINTER SWAP link-area !
    run-queue ! ;

  : make-run-queue-link ?( area-addr prev-link -- new-link)
    SWAP allocate-run-queue-link
    TUCK link-area !
    2DUP link-prev !
    TUCK link-next SWAP link-next @ SWAP !
    DUP DUP link-next @ link-prev !
    DUP DUP link-prev @ link-next ! ;

  : do-schedule ?( area-addr -- ) run-queue ['] make-run-queue-link @! ;
  : area>flags ?( area-addr -- flags-addr) $28 + ;
  : asleep? ?( area-addr --) area>flags @ 2 TEST-FLAG ;
  : set-sleep-flag ?( flags -- flags) 2 SET-FLAG ;
  : unset-sleep-flag ?( flags -- flags) 2 UNSET-FLAG ;
  : set-asleep ?( area-addr --) area>flags ['] set-sleep-flag @! ;
  : set-awake ?( area-addr --) area>flags ['] unset-sleep-flag @! ;
  : SCHEDULE ?( pid --)
    PID>AREA 0RETURN DUP asleep? IF DUP set-awake do-schedule ELSE DROP THEN ;

  6 IPB-ADDR CONSTANT quantum-addr
  : TICK-QUANTUM ?( --) quantum-addr @ 1 SAT- quantum-addr ! ;

  : YIELD ?( --)
    run-queue @ link-next @ DUP run-queue ! link-area @
    ." Context switch to " DUP . CR
    CONTEXT-SWITCH ;
  :NONAME ?( --) quantum-addr @ 0= IF YIELD THEN ; IS MAYBE-YIELD

  : remove-run-queue-link ?( link-addr -- next-addr)
    DUP link-next @ SWAP link-prev @ 2DUP
    SWAP link-prev ! SWAP DUP ROT link-next ! ;
  : remove-from-run-queue ?( --) run-queue ['] remove-run-queue-link @! ;
  : SLEEP ?( --) PROCESS-POINTER set-asleep remove-from-run-queue YIELD ;

  : remove-from-process-list ?( pid addr --)
    BEGIN
      DUP @ ABORT" remove-from-process-list called on non-existent PID"
      2DUP @ CELL+ @
      = IF NIP DUP @ @ SWAP ! LEAVE THEN
      @
    AGAIN ;
  : remove-from-process-table ?( pid --)
    DUP $ffff AND CELLS process-table + remove-from-process-list ;
  : PID ?( -- pid) PROCESS-POINTER @ ;
  : DIE ?( --) remove-from-run-queue PID remove-from-process-table
    run-queue @ link-area @ CONTEXT-SWITCH ;

  :NONAME [ DROP HERE ] R> EXECUTE QUIT ;
  CONSTANT entrypoint

  : make-process-area ?( u_k ... u_1 k xt pid -- addr)
    allocate-process-area
    TUCK ! \ Process ID
    TUCK $1f8 + ! \ XT
    OVER 0 DO
      I 2 + PICK
      OVER $3f0 + I CELLS - !
    LOOP
    $504f544b43415453 OVER $3f8 + ! \ Stack top canary
    TRUE OVER 8 + ! \ Start of Source
    4 OVER $28 + ! \ Flags; we start asleep so SCHEDULE will schedule us
    ['] GET-ABORT OVER $40 + ! \ ABORT
    ['] GET-BP    OVER $48 + ! \ BP
    ['] GET-EMIT  OVER $50 + ! \ EMIT
    ['] GET-QUIT  OVER $58 + ! \ QUIT
    entrypoint OVER $e8 + ! \ Stored RSI
    OVER CELLS $3f8 SWAP - OVER + OVER $f0 + ! \ Stored RSP
    DUP $1f8 + OVER $f8 + ! \ Stored RBP
    >R DISCARD R> ;
  : make-process-entry ?( addr pid -- addr)
    allocate-process-entry
    TUCK 8 + !
    TUCK $10 + ! ;

  : add-to-process-table-entry ?( entry addr --)
    BEGIN DUP @ WHILE @ REPEAT ! ;
  : add-to-process-table ?( pid addr --)
    OVER make-process-entry
    SWAP $ffff AND CELLS
    process-table + add-to-process-table-entry ;

  \ PIDs are always positive, just for mental convenience.
  : random-pid ?( -- pid) RAND-WORD 1 $3f LSHIFT 1- AND ;
  : make-pid ?( -- pid)
    random-pid BEGIN DUP PID>AREA WHILE DROP random-pid REPEAT ;

  : PID>NAME ?( pid -- addr len) PID>AREA DUP $60 + @ SWAP $68 + @ ;
  : SET-PROCESS-NAME ?( addr len --)
    PROCESS-POINTER $68 + ! PROCESS-POINTER $60 + ! ;

  : SPAWN ?( u_k ... u_1 k xt --)
    make-pid DUP
    >R make-process-area R>
    2DUP SWAP add-to-process-table
    SCHEDULE CONTEXT-SWITCH ;
END-MODULE( DIE PID PID>AREA PID>NAME SCHEDULE SET-PROCESS-NAME SETUP-SCHEDULER
  SLEEP SPAWN TICK-QUANTUM YIELD )

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
