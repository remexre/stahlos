#!/bin/bash

exec qemu-system-aarch64 \
	-machine virt \
	-cpu cortex-a57 \
	-bios ~/Downloads/u-boot/u-boot
