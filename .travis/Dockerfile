FROM alpine:edge

RUN apk add --no-cache binutils expect gcc grub grub-bios grub-efi libc-dev make mtools nasm qemu-system-x86_64 xorriso
RUN apk add --no-cache cargo && \
	cargo install mdbook --vers "0.3.1" && \
	install /root/.cargo/bin/mdbook /usr/local/bin && \
	apk del cargo && \
	rm -r /root/.cargo
# RUN apk add --no-cache curl && \
	# curl -L https://github.com/rust-lang-nursery/mdBook/releases/download/v0.3.0/mdbook-v0.3.0-x86_64-unknown-linux-musl.tar.gz | tar zxC /usr/local/bin && \
	# apk del curl

VOLUME /code
WORKDIR /code
