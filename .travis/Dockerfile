FROM rust:latest

RUN git clone https://github.com/ehuss/mdBook.git --branch update-pulldown-cmark && \
	cd mdBook && \
	cargo build --release && \
	cp target/release/mdbook /usr/local/bin/mdbook && \
	apt-get update && \
	apt-get install -y --no-install-recommends grub nasm xorriso && \
	rm -rf /var/lib/apt/lists/*
VOLUME /code
WORKDIR /code
