add-symbol-file out/stahlos.sym 0

break *0
break bp_handler
break ud_handler
break df_handler
break gp_handler
break pf_handler

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
