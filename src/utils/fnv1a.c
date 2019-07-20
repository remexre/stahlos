/** Computes the 64-bit FNV1a hash of argv[1]. */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

unsigned long long fnv1a64(const char* cstr);

int main(int argc, char** argv) {
	if(argc != 2) {
		fprintf(stderr, "Usage: %s <string>\n", argc ? argv[0] : "fnv1a");
		return EXIT_FAILURE;
	}

	// Test vectors from Appendix C of draft-eastlake-fnv-16.
	assert(fnv1a64("") == 0xcbf29ce484222325);
	assert(fnv1a64("a") == 0xaf63dc4c8601ec8c);
	assert(fnv1a64("foobar") == 0x85944171f73967e8);

	printf("0x%016llx\n", fnv1a64(argv[1]));
	return EXIT_SUCCESS;
}

unsigned long long fnv1a64(const char* cstr) {
	const unsigned long long OFFSET_BASIS = 0xcbf29ce484222325;
	const unsigned long long PRIME = 1099511628211;

	unsigned long long hash = OFFSET_BASIS;
	size_t len = strlen(cstr);
	for(size_t i = 0; i < len; i++) {
		hash ^= cstr[i];
		hash *= PRIME;
	}
	return hash;
}
