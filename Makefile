DESTDIR ?= /media/stahlos
NASM ?= nasm
NASMFLAGS += -gdwarf -Werror

ASM_UNITS += amd64/multiboot2
ASM_UNITS += amd64/start32
ASM_UNITS += amd64/structures
ASM_UNITS += amd64/start
ASM_UNITS += amd64/alloc
ASM_UNITS += amd64/int
ASM_UNITS += amd64/panic
ASM_UNITS += amd64/devices/pic8259
ASM_UNITS += amd64/devices/uart8250
ASM_UNITS += amd64/kernel/builtins
ASM_UNITS += amd64/kernel/error_handling
ASM_UNITS += amd64/kernel/interpret
ASM_UNITS += amd64/kernel/pseudobuiltins

FORTH_UNITS += acpi
# FORTH_UNITS += ata-pio
# FORTH_UNITS += gpt
# FORTH_UNITS += mbr
# FORTH_UNITS += pcie

MISC_UTILS += fnv1a

ASM_OBJS = $(patsubst %,tmp/%.o,$(ASM_UNITS))
FORTH_SRCS = $(patsubst %,src/forth/%.f,$(FORTH_UNITS))

all: kernel image docs
clean:
	rm -rf tmp out
docs:
	mdbook build
help:
	@echo >&2 'Targets:'
	@echo >&2 '  all     - Does kernel, image, and docs'
	@echo >&2 '  clean   - Removes temporary and output files'
	@echo >&2 '  docs    - Builds documentation'
	@echo >&2 '  image   - Builds a boot image to out/stahlos.img'
	@echo >&2 '  install - Installs the kernel and modules to DESTDIR'
	@echo >&2 '  kernel  - Builds the kernel to out/stahlos.elf'
	@echo >&2 '  run     - Runs the boot image in Bochs'
	@echo >&2 '  watch   - Watches source files, recompiling on changes'
image: out/stahlos.img
install: out/stahlos.elf $(FORTH_SRCS)
	@mkdir -p $(DESTDIR)/boot/mods
	cp $(FORTH_SRCS) $(DESTDIR)/boot/mods/
	cp out/stahlos.elf $(DESTDIR)/boot/
kernel: out/stahlos.elf
run: out/stahlos.img
	bochs -f bochsrc.txt -q
utils: $(patsubst %,out/utils/%,$(MISC_UTILS))
watch:
	watchexec -cre asm,cfg,inc,ld,md make
.PHONY: all clean docs help image install kernel run utils watch

disas: out/stahlos-unstripped.elf
	objdump -M intel -d $< | less
find-bochs-bps:
	rg '^[^;]*xchg\s*bx\s*,\s*bx'
run-qemu: out/stahlos.img
	qemu-system-x86_64 \
		-accel kvm \
		-d cpu_reset -d guest_errors -d int \
		-debugcon stdio \
		-drive format=raw,file=out/stahlos.img,if=ide,media=disk \
		-m 64M \
		-machine q35
.PHONY: disas find-bochs-bps run-qemu

ci: all
	chown $(shell stat -c '%u:%g' Makefile) -R tmp out
.PHONY: ci

out/stahlos.img: out/stahlos.elf src/misc/grub.cfg $(FORTH_SRCS)
	@grub-file --is-x86-multiboot2 out/stahlos.elf
	@mkdir -p tmp/isodir/boot/grub
	cp out/stahlos.elf tmp/isodir/boot/stahlos.elf
	cp src/misc/grub.cfg tmp/isodir/boot/grub/grub.cfg
	@mkdir -p tmp/isodir/boot/mods
	cp $(FORTH_SRCS) tmp/isodir/boot/mods
	@mkdir -p $(dir $@)
	grub-mkrescue -o $@ tmp/isodir
out/utils/%: tmp/utils/%.o
	@mkdir -p $(dir $@)
	$(CC) -o $@ $^

out/stahlos.elf out/stahlos.sym: out/stahlos-unstripped.elf
	@mkdir -p $(dir $@)
	cp out/stahlos-unstripped.elf out/stahlos.elf
	cp out/stahlos-unstripped.elf out/stahlos.sym
	strip --only-keep-debug out/stahlos.sym
	strip out/stahlos.elf

out/stahlos-unstripped.elf: src/misc/linker.ld $(ASM_OBJS)
	@mkdir -p $(dir $@)
	ld -o $@ -T $^ -n -z max-page-size=0x1000

tmp/%.o: src/%.asm
	@mkdir -p $(dir $@)
	$(NASM) -felf64 -o $@ $< $(NASMFLAGS)
tmp/utils/%.o: src/utils/%.c
	@mkdir -p $(dir $@)
	$(CC) -c -o $@ $(CFLAGS) $^
