UNITS+=args
UNITS+=main
UNITS+=util
UNITS+=vm

ifneq ($(CALLED_FROM_MAIN_MAKEFILE),1)
$(error This Makefile should be called from the top-level Makefile)
endif

all: $(DESTDIR)/stahlos.elf
.PHONY: all

$(DESTDIR)/stahlos.elf: $(patsubst %,$(TMPDIR)/%.o,$(UNITS))
	@mkdir -p $(dir $@)
	$(CC) -g -o $@ $^

$(TMPDIR)/%.o: %.c
	@mkdir -p $(dir $@)
	$(CC) -c -g -O2 -std=c11 -Wall -Wextra -Werror -o $@ $^
