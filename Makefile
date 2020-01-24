DESTDIR?=out
TMPDIR?=tmp
WATCH_TARGET=all

TARGET_ARCH=$(shell uname -m)

ARCHES+=aarch64

EXPS+=aarch64-simple

all: help
clean:
ifeq ($(DESTDIR),out)
	[[ ! -d out ]] || rm -r out
endif
ifeq ($(TMPDIR),tmp)
	[[ ! -d tmp ]] || rm -r tmp
endif
exp: $(patsubst %,exp/%,$(EXPS))
help:
	@echo >&2 'Targets:'
	@echo >&2 '  clean  - Removes temporary and output files'
	@echo >&2 '  exp    - Builds experiments (this may not, in general, work)'
	@echo >&2 '  kernel - Builds the kernel for TARGET_ARCH (default: $(TARGET_ARCH))'
	@echo >&2 '  watch  - Watches source files, recompiling on changes'
kernel: $(DESTDIR)/stahlos.elf
watch:
	watchexec -r -w Makefile -w exp -w src $(MAKE) $(WATCH_TARGET)
.PHONY: all clean exp help kernel watch

$(DESTDIR)/stahlos.elf: $(DESTDIR)/kernel-$(TARGET_ARCH)/stahlos.elf
	@mkdir -p $(dir $@)
	@cp $< $@

# Despite "recursive make considered harmful," we actually want to isolate
# both experiments and each arch's kernel from the rest of the source tree.
# (And furthermore, we want to allow separate compilation of each of these:
# experiments, since they may be arbitrarily broken at any given moment, and
# arches, since the user probably doesn't want all of them.)
define build-exp =
exp/$(1):
	@$(MAKE) -C exp/$(1) CALLED_FROM_MAIN_MAKEFILE=1 \
		DESTDIR=$(abspath $(DESTDIR)/exp/$(1)) \
		TMPDIR=$(abspath $(TMPDIR)/exp/$(1))
endef
$(foreach EXP,$(EXPS),$(eval $(call build-exp,$(EXP))))
$(foreach EXP,$(EXPS),.PHONY: exp/$(EXP))

define build-kernel-arch =
$(DESTDIR)/kernel-$(1)/stahlos.elf:
	@mkdir -p $$(dir $$@)
	@$(MAKE) -C src/kernel-$(1) CALLED_FROM_MAIN_MAKEFILE=1 \
		DESTDIR=$(abspath $(DESTDIR)/kernel-$(1)) \
		TMPDIR=$(abspath $(TMPDIR)/kernel-$(1))
endef
$(foreach ARCH,$(ARCHES),$(eval $(call build-kernel-arch,$(ARCH))))
$(foreach ARCH,$(ARCHES),.PHONY: $(DESTDIR)/kernel-$(ARCH)/stahlos.elf)
