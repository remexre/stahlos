UNITS+=builtins
UNITS+=main

UNITS+=devices/rk3399/uart

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

all: image
image: $(DESTDIR)/stahlos.img
kernel: $(DESTDIR)/stahlos.bin $(DESTDIR)/stahlos.elf
.PHONY: all image kernel

$(DESTDIR)/stahlos.img: $(TMPDIR)/partition.img
	@mkdir -p $(dir $@)
	truncate -s 64M $@
	dd if=$< of=$@ seek=1 bs=1M conv=notrunc
	sgdisk -n 1:2048:131038 $@
$(TMPDIR)/partition.img: $(TMPDIR)/image/boot/boot.scr $(TMPDIR)/image/boot/stahlos.bin
	@mkdir -p $(dir $@)
	truncate -s 64495K $@
	mke2fs -d $(TMPDIR)/image -L StahlOS $@

$(TMPDIR)/image/boot/boot.scr: boot.txt
	@mkdir -p $(dir $@)
	mkimage -A arm64 -T script -C None -n StahlOS -d $< $@

$(TMPDIR)/image/boot/stahlos.bin: $(DESTDIR)/stahlos.bin
	@mkdir -p $(dir $@)
	@cp $< $@

$(DESTDIR)/stahlos.bin: $(DESTDIR)/stahlos.elf
	@mkdir -p $(dir $@)
	$(AARCH64_OBJCOPY) $< -O binary $@

$(DESTDIR)/stahlos.elf: linker.ld $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(AARCH64_LD) -o $@ -T $^

$(TMPDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(AARCH64_AS) -o $@ $^
