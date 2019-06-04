Forth Naming
============

Most commonly-used words' names are taken from the Forth 2012 standard at [forth-standard.org](https://forth-standard.org/). Some other names have been changed in an attempt to make the naming more intuitive and consistent.

If a word shouldn't be used by the programmer, but is instead likely to be inserted by `[COMPILE]` or the like, it is surrounded by parentheses, for example `(EXIT)`.

If a word is an immediate version of a normally non-immediate word, it is surrounded by brackets, for example `[']`.

If a word writes directly to the data space, it should be suffixed with a comma, for example `HEADER,`.
