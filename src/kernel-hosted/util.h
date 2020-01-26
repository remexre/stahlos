#ifndef STAHLOS__KERNEL_HOSTED__UUID_H
#define STAHLOS__KERNEL_HOSTED__UUID_H 1

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

typedef struct buf {
	uint8_t* bytes;
	size_t cap;
	size_t len;
} buf;

buf read_file(const char* path);
void free_buf(buf);

typedef struct uuid {
	uint64_t lo;
	uint64_t hi;
} uuid;

uuid uuid_new_v4(void);
void uuid_write(FILE*, uuid);

inline static uint8_t hash_pid(uuid uuid) {
	uint64_t hash = 5381;
	uint8_t* bytes = (uint8_t*) &uuid.lo;
	for(int i = 0; i < 16; i++)
		hash = (hash * 33) ^ bytes[i];
	return (uint8_t) hash;
}

inline static uint64_t round_up(uint64_t n) {
	return (n + 7) & ~7;
}

#endif
