FROM alpine:edge

# Get mdbook; until https://github.com/rust-lang-nursery/mdBook/pull/898 is merged, we wanna build
# from source.
RUN apk add --no-cache cargo && \
	cargo install --git https://github.com/ehuss/mdBook.git --branch update-pulldown-cmark mdbook && \
	apk del cargo && \
	mv $HOME/.cargo/bin/mdbook /usr/local/bin/mdbook && \
	rm -rf $HOME/.cargo

RUN apk add --no-cache binutils gcc git grub grub-bios grub-efi libc-dev make mtools nasm xorriso
VOLUME /code
WORKDIR /code
