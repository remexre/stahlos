TARGET_ARCH?=$(shell uname -m)
DESTDIR?=out
TMPDIR?=tmp
WATCH_TARGET?=help

AARCH64_AS?=$(or $(shell which aarch64-none-elf-as 2>/dev/null),$(CROSS_COMPILE)as)
AARCH64_LD?=$(or $(shell which aarch64-none-elf-ld 2>/dev/null),$(CROSS_COMPILE)ld)
AARCH64_OBJCOPY?=$(or $(shell which aarch64-none-elf-objcopy 2>/dev/null),$(CROSS_COMPILE)objcopy)

ARCHES+=aarch64
ARCHES+=hosted

# EXPS+=aarch64-simple

all: image
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
	@echo >&2 '  image  - Builds a disk image for TARGET_ARCH'
	@echo >&2 '  kernel - Builds the kernel for TARGET_ARCH'
	@echo >&2 '  watch  - Watches source files, running WATCH_TARGET on changes'
	@echo >&2 ''
	@echo >&2 'Environment Variables:'
	@echo >&2 '  TARGET_ARCH=$(TARGET_ARCH)'
	@echo >&2 '  DESTDIR=$(DESTDIR)'
	@echo >&2 '  TMPDIR=$(TMPDIR)'
	@echo >&2 '  WATCH_TARGET=$(WATCH_TARGET)'
	@echo >&2 ''
	@echo >&2 '  AARCH64_AS=$(AARCH64_AS)'
	@echo >&2 '  AARCH64_LD=$(AARCH64_LD)'
	@echo >&2 '  AARCH64_OBJCOPY=$(AARCH64_OBJCOPY)'
image: image-$(TARGET_ARCH)
kernel: kernel-$(TARGET_ARCH)
watch:
	watchexec -r -w Makefile -w exp -w src $(MAKE) $(WATCH_TARGET)
.PHONY: all clean exp help kernel watch

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
.PHONY: exp/$(1)
endef
$(foreach EXP,$(EXPS),$(eval $(call build-exp,$(EXP))))

define build-arch =
image-$(1):
	@$(MAKE) -C src/kernel-$(1) image \
		CALLED_FROM_MAIN_MAKEFILE=1 \
		DESTDIR=$(abspath $(DESTDIR)/kernel-$(1)) \
		TMPDIR=$(abspath $(TMPDIR)/kernel-$(1)) \
		AARCH64_AS=$(AARCH64_AS) \
		AARCH64_LD=$(AARCH64_LD) \
		AARCH64_OBJCOPY=$(AARCH64_OBJCOPY)
kernel-$(1):
	@$(MAKE) -C src/kernel-$(1) kernel \
		CALLED_FROM_MAIN_MAKEFILE=1 \
		DESTDIR=$(abspath $(DESTDIR)/kernel-$(1)) \
		TMPDIR=$(abspath $(TMPDIR)/kernel-$(1)) \
		AARCH64_AS=$(AARCH64_AS) \
		AARCH64_LD=$(AARCH64_LD) \
		AARCH64_OBJCOPY=$(AARCH64_OBJCOPY)
.PHONY: image-$(1) kernel-$(1)
endef
$(foreach ARCH,$(ARCHES),$(eval $(call build-arch,$(ARCH))))
