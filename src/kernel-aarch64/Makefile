UNITS+=builtins
UNITS+=main

UNITS+=devices/rk3399/uart

ifneq ($(CALLED_FROM_MAIN_MAKEFILE),1)
$(error This Makefile should be called from the top-level Makefile)
endif

all: $(DESTDIR)/stahlos.elf
.PHONY: all

$(DESTDIR)/stahlos.elf: linker.ld $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(AARCH64_LD) -o $@ -T $^

$(TMPDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(AARCH64_AS) -o $@ $^
