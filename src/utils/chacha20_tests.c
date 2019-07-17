#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

typedef uint8_t u8;
typedef uint32_t u32;
typedef u32 state[16];
typedef u32 nonce[3];
typedef u32 block[16];

u32 rotate_left_7(u32 x);
void quarter_round(u32* a, u32* b, u32* c, u32* d);
void two_rounds(state);
void twenty_rounds_no_add(state);
void twenty_rounds(state);
void encrypt(const u8*, u8*, size_t);

const size_t NUM_RANDOM_TESTS = 1000;
u32 rotate_left_reference(u32 x, u32 n);

const char* test = "<not set>";
void expect(u32 g, u32 e);
void expect_bytes(const u8* g, const u8* e, size_t l);
void expect_state(state g, state e);
void pass(void);

// Test vectors from RFC8439.
int main(void) {
	test = "Rotate Left";
	srand(time(NULL));
	for(size_t i = 0; i < NUM_RANDOM_TESTS; i++) {
		u32 x = rand();
		expect(rotate_left_7(x), rotate_left_reference(x, 7));
	}
	pass();

	test = "Section 2.1.1"; // Test Vector for the ChaCha Quarter Round
	u32 a = 0x11111111, b = 0x01020304, c = 0x9b8d6f43, d = 0x01234567;
	quarter_round(&a, &b, &c, &d);
	expect(a, 0xea2a92f4);
	expect(b, 0xcb1cf8ce);
	expect(c, 0x4581472e);
	expect(d, 0x5881c4bb);
	pass();

	test = "Section 2.2.1"; // Test Vector for the Quarter Round on the ChaCha State
	state s, t;
	s[0x0] = 0x879531e0; s[0x1] = 0xc5ecf37d; s[0x2] = 0x516461b1; s[0x3] = 0xc9a62f8a;
	s[0x4] = 0x44c20ef3; s[0x5] = 0x3390af7f; s[0x6] = 0xd9fc690b; s[0x7] = 0x2a5f714c;
	s[0x8] = 0x53372767; s[0x9] = 0xb00a5631; s[0xa] = 0x974c541a; s[0xb] = 0x359e9963;
	s[0xc] = 0x5c971061; s[0xd] = 0x3d631689; s[0xe] = 0x2098d9d6; s[0xf] = 0x91dbd320;
	t[0x0] = 0x879531e0; t[0x1] = 0xc5ecf37d; t[0x2] = 0xbdb886dc; t[0x3] = 0xc9a62f8a;
	t[0x4] = 0x44c20ef3; t[0x5] = 0x3390af7f; t[0x6] = 0xd9fc690b; t[0x7] = 0xcfacafd2;
	t[0x8] = 0xe46bea80; t[0x9] = 0xb00a5631; t[0xa] = 0x974c541a; t[0xb] = 0x359e9963;
	t[0xc] = 0x5c971061; t[0xd] = 0xccc07c79; t[0xe] = 0x2098d9d6; t[0xf] = 0x91dbd320;
	quarter_round(&s[2], &s[7], &s[8], &s[13]);
	expect_state(s, t);

	test = "Section 2.3.2, Part 1"; // Test Vector for the ChaCha20 Block Function
	s[0x0] = 0x61707865; s[0x1] = 0x3320646e; s[0x2] = 0x79622d32; s[0x3] = 0x6b206574;
	s[0x4] = 0x03020100; s[0x5] = 0x07060504; s[0x6] = 0x0b0a0908; s[0x7] = 0x0f0e0d0c;
	s[0x8] = 0x13121110; s[0x9] = 0x17161514; s[0xa] = 0x1b1a1918; s[0xb] = 0x1f1e1d1c;
	s[0xc] = 0x00000001; s[0xd] = 0x09000000; s[0xe] = 0x4a000000; s[0xf] = 0x00000000;
	t[0x0] = 0x837778ab; t[0x1] = 0xe238d763; t[0x2] = 0xa67ae21e; t[0x3] = 0x5950bb2f;
	t[0x4] = 0xc4f2d0c7; t[0x5] = 0xfc62bb2f; t[0x6] = 0x8fa018fc; t[0x7] = 0x3f5ec7b7;
	t[0x8] = 0x335271c2; t[0x9] = 0xf29489f3; t[0xa] = 0xeabda8fc; t[0xb] = 0x82e46ebd;
	t[0xc] = 0xd19c12b4; t[0xd] = 0xb04e16de; t[0xe] = 0x9e83d0cb; t[0xf] = 0x4e3c50a2;
	twenty_rounds_no_add(s);
	expect_state(s, t);

	test = "Section 2.3.2, Part 2"; // Test Vector for the ChaCha20 Block Function
	s[0x0] = 0x61707865; s[0x1] = 0x3320646e; s[0x2] = 0x79622d32; s[0x3] = 0x6b206574;
	s[0x4] = 0x03020100; s[0x5] = 0x07060504; s[0x6] = 0x0b0a0908; s[0x7] = 0x0f0e0d0c;
	s[0x8] = 0x13121110; s[0x9] = 0x17161514; s[0xa] = 0x1b1a1918; s[0xb] = 0x1f1e1d1c;
	s[0xc] = 0x00000001; s[0xd] = 0x09000000; s[0xe] = 0x4a000000; s[0xf] = 0x00000000;
	t[0x0] = 0xe4e7f110; t[0x1] = 0x15593bd1; t[0x2] = 0x1fdd0f50; t[0x3] = 0xc47120a3;
	t[0x4] = 0xc7f4d1c7; t[0x5] = 0x0368c033; t[0x6] = 0x9aaa2204; t[0x7] = 0x4e6cd4c3;
	t[0x8] = 0x466482d2; t[0x9] = 0x09aa9f07; t[0xa] = 0x05d7c214; t[0xb] = 0xa2028bd9;
	t[0xc] = 0xd19c12b5; t[0xd] = 0xb94e16de; t[0xe] = 0xe883d0cb; t[0xf] = 0x4e3c50a2;
	twenty_rounds(s);
	expect_state(s, t);

	return 0;
}

u32 rotate_left_reference(u32 x, u32 n) {
	return (x << n) | (x >> (32 - n));
}

void expect(u32 g, u32 e) {
	if(g != e) {
		printf("%s: Expected 0x%08x to equal 0x%08x.\n", test, g, e);
		exit(1);
	}
}

void pass(void) {
	printf("%s: OK\n", test);
	test = "<not set>";
}

void expect_bytes(const u8* g, const u8* e, size_t l) {
	for(size_t i = 0; i < l; i++) {
		if(g[i] != e[i]) {
			printf("%s: Expected 0x%02x to equal 0x%02x.\n", test, g[i], e[i]);
			for(size_t j = 0; j < l; j++)
				printf("0x%lx - 0x%02x vs 0x%02x %c\n", j, g[j], e[j], i == j ? '*' : ' ');
			exit(1);
		}
	}
	pass();
}

void expect_state(u32 g[16], u32 e[16]) {
	for(size_t i = 0; i < 16; i++) {
		if(g[i] != e[i]) {
			printf("%s: Expected 0x%08x to equal 0x%08x.\n", test, g[i], e[i]);
			for(size_t j = 0; j < 16; j++)
				printf("0x%lx - 0x%08x vs 0x%08x %c\n", j, g[j], e[j], i == j ? '*' : ' ');
			exit(1);
		}
	}
	pass();
}
