predefined >r ( x1 --) ( R: -- x1)
predefined r> ( -- x1) ( R: x1 --)
predefined @ ( addr -- x1)
predefined cell+ ( addr -- addr)
predefined dup ( x1 -- x1 x1)
predefined process-table ( -- addr)
predefined swap ( x1 x2 -- x2 x1)

: 2>r ( x1 x2 --) ( -- R: x1 x2) swap >r >r ;
: 2r> ( -- x1 x2) ( R: x1 x2 --) r> r> swap ;
: over ( x1 x2 -- x1 x2 x1) >r dup r> swap ;
: 2dup ( x1 x2 -- x1 x2 x1 x2) over over ;
: set-source todo ;
: source ( -- c-addr len ) process-table cell+ dup @ swap cell+ @ ;

: evaluate ( i*x c-addr len -- j*y)
  source 2>r set-source
  begin
    parse-name dup
  while
    2dup find-header ( addr len -- 0 | header-addr)
    dup if
      >r 2drop r>
      dup header>xt swap header>immediate? get-state or if
        execute
      else
        compile,
      endif
    else
      drop 2dup >number ( addr len -- x1 -1 | 0 0) if
        get-state not if literal endif
        >r 2drop r>
      else
        ." Not found: " type
      endif
    endif
  repeat
  2r> set-source ;
