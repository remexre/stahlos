bits 64

global dictionary_start
global dictionary_end

[section .dictionary nobits]

dictionary_start:
resb (62 * 1024 * 1024)
dictionary_end:

; vi: cc=80 ft=nasm
