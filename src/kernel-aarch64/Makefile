UNITS+=main

ifneq ($(CALLED_FROM_MAIN_MAKEFILE),1)
$(error This Makefile should be called from the top-level Makefile)
endif

all: $(DESTDIR)/stahlos.elf
.PHONY: all

$(DESTDIR)/stahlos.elf: $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(LD) -o $@ $^

$(TMPDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(AS) -o $@ $^
