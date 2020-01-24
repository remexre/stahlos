UNITS+=main
UNITS+=builtins
UNITS+=linux_functions
UNITS+=linux_words
UNITS+=utils

ifneq ($(CALLED_FROM_MAIN_MAKEFILE),1)
$(error This Makefile should be called from the top-level Makefile)
endif

all: $(DESTDIR)/aarch64-simple
.PHONY: all

$(DESTDIR)/aarch64-simple: $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(LD) -o $@ $^

$(TMPDIR)/%.o: %.s
	@mkdir -p $(dir $@)
	$(AS) -o $@ $^
