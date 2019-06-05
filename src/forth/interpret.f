\ A reimplementation of INTERPRET, since the one in machine code is a bit
\ magical.

: interpret ( -- i*u )
  begin
    parse-name dup
  while
    2dup find dup if
      get-state -
      -rot 2dup if execute else compile, endif
    else
      drop 2dup >signed-number if
        ['] (literal) compile, , 2drop
      else
        ." Undefined word: " type quit
      endif
    endif
  repeat ;

\ vim: set cc=80 ft=forth ss=2 sw=2 ts=2 et :
