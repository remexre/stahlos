#include "util.h"

#include <errno.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static bool srand_called = false;

buf read_file(const char* path) {
	FILE* file = fopen(path, "rb");
	if(!file) {
		char* err = strerror(errno);
		fprintf(stderr, "Failed to open file %s: %s\n", path, err);
		exit(EXIT_FAILURE);
	}

	buf buf = {
		.bytes = malloc(4096),
		.cap = 4096,
		.len = 0
	};
	while(1) {
		buf.len += fread(&buf.bytes[buf.len], 1, buf.cap - buf.len, file);
		if(buf.cap == buf.len) {
			buf.cap *= 2;
			buf.bytes = realloc(buf.bytes, buf.cap);
		} else if(feof(file)) {
			break;
		} else {
			char* err = strerror(errno);
			fprintf(stderr, "Failed to read from file %s: %s\n", path, err);
			exit(EXIT_FAILURE);
		}
	}

	if(fclose(file) < 0) {
		char* err = strerror(errno);
		fprintf(stderr, "Failed to close file %s: %s\n", path, err);
		exit(EXIT_FAILURE);
	}

	return buf;
}

void free_buf(buf buf) {
	free(buf.bytes);
}

uuid uuid_new_v4(void) {
	if(!srand_called) {
		srand_called = true;
		srand(time(0));
	}

	uuid out;
	out.lo = (((uint64_t) rand()) << 32) | ((uint64_t) rand());
	out.hi = (((uint64_t) rand()) << 32) | ((uint64_t) rand());
	out.hi &= 0xffffffffffff0fff;
	out.hi |= 0x0000000000004000;
	out.lo &= 0x3fffffffffffffff;
	out.lo |= 0x8000000000000000;
	return out;
}

void uuid_write(FILE* file, uuid uuid) {
	uint32_t a = (uint32_t)(uuid.hi >> 32);
	uint16_t b = (uint16_t)(uuid.hi >> 16);
	uint16_t c = (uint16_t) uuid.hi;
	uint16_t d = (uint16_t)(uuid.lo >> 48);
	uint64_t e = uuid.lo & ((((uint64_t) 1) << 48) - 1);
	fprintf(file, "%08x-%04x-%04x-%04x-%012lx", a, b, c, d, e);
}
