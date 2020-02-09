UNITS+=main

UNITS+=format
UNITS+=panic

UNITS+=forth/builtins
UNITS+=forth/pseudobuiltins

UNITS+=devices/qemuvirt/uart
UNITS+=devices/rk3399/uart2

ifneq ($(CALLED_FROM_MAIN_MAKEFILE),1)
$(error This Makefile should be called from the top-level Makefile)
endif

ifeq ($(IGNORE_MISSING_PROGRAMS),)
ifeq ($(shell which mke2fs 2>/dev/null),)
$(error mke2fs not found (install e2fsprogs))
endif

ifeq ($(shell which mkimage 2>/dev/null),)
$(error mkimage not found (install uboot-tools))
endif

ifeq ($(shell which sgdisk 2>/dev/null),)
$(error sgdisk not found (install gptfdisk))
endif
endif

EMU = qemu-system-aarch64 $(AARCH64_QEMU_FLAGS)
EMU += -machine virt -cpu cortex-a53
EMU += -m 4G
EMU += -kernel $(DESTDIR)/stahlos.elf
# TODO: stahlos.img

all: image
debug: $(DESTDIR)/stahlos.elf $(TMPDIR)/stahlos-unstripped.elf
	$(AARCH64_GDB) \
		-ex "add-symbol-file $(TMPDIR)/stahlos-unstripped.elf" \
		-ex "target remote | $(EMU) -S -gdb stdio" \
		-ex "break forth_impl_debug" \
		-ex "break panic" \
		-ex "layout regs" \
		-x misc/gdb_extensions.py
emu: $(DESTDIR)/stahlos.elf $(DESTDIR)/stahlos.img
	$(EMU) -serial stdio -display none
image: $(DESTDIR)/stahlos.img
kernel: $(DESTDIR)/stahlos.bin $(DESTDIR)/stahlos.elf
.PHONY: all emu image kernel

$(DESTDIR)/stahlos.img: $(TMPDIR)/partition.img
	@mkdir -p $(dir $@)
	@[[ ! -f $@ ]] || rm $@
	truncate -s 64M $@
	dd if=$< of=$@ seek=1 bs=1M conv=notrunc
	sgdisk -n 1:2048:131038 $@
$(TMPDIR)/partition.img: $(TMPDIR)/image/boot/boot.scr $(TMPDIR)/image/boot/stahlos.elf
	@mkdir -p $(dir $@)
	@[[ ! -f $@ ]] || rm $@
	truncate -s 64495K $@
	mke2fs -d $(TMPDIR)/image -L StahlOS $@

$(TMPDIR)/image/boot/boot.scr: boot.txt
	@mkdir -p $(dir $@)
	mkimage -A arm64 -T script -C None -n StahlOS -d $< $@

$(TMPDIR)/image/boot/stahlos.elf: $(DESTDIR)/stahlos.elf
	@mkdir -p $(dir $@)
	@cp $< $@

$(DESTDIR)/stahlos.bin: $(DESTDIR)/stahlos.elf
	@mkdir -p $(dir $@)
	$(AARCH64_OBJCOPY) $< -O binary $@

$(DESTDIR)/stahlos.elf: $(TMPDIR)/stahlos-unstripped.elf
	@mkdir -p $(dir $@)
	$(AARCH64_STRIP) -o $@ $<

$(TMPDIR)/stahlos-unstripped.elf: linker.ld $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(AARCH64_LD) -o $@ -T $^ -N -z max-page-size=4096

$(TMPDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(AARCH64_AS) -g -o $@ $<

$(TMPDIR)/main.o: init.fth
