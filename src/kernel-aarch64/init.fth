CREATE : ] CREATE ] (;) [ ' (:) SET-DOES
CREATE ; ' [ COMPILE, ] (LITERAL) (;) COMPILE, (LITERAL) (:) SET-DOES (;) [ ' (:) SET-DOES IMMEDIATE

CREATE repl:buffer 80 ALLOT
: repl:read-line repl:buffer DUP 80 READ-LINE ;
: repl:one repl:read-line 0 EVALUATE ;
: repl:rec repl:one repl:rec ;

repl:rec
1638 .HEX CR
