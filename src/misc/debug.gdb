add-symbol-file out/stahlos.sym 0

break *0
break bp_handler

source src/misc/gdb_tools.py

target remote localhost:1234

define l
	layout asm
	layout regs
	focus cmd
end
define nsi
	next
	stepi
end

continue

# vi: ft=gdb
