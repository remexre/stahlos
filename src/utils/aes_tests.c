#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef struct {
	u8 b0;  u8 b1;  u8 b2;  u8 b3;
	u8 b4;  u8 b5;  u8 b6;  u8 b7;
	u8 b8;  u8 b9;  u8 b10; u8 b11;
	u8 b12; u8 b13; u8 b14; u8 b15;
} u128;
#define U128(B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15) \
	((u128) { B0, B1, B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12, B13, B14, B15 })

void aes_do_encrypt(u128* key, u128* plain, u128* cipher);

static bool u128_eq(u128 l, u128 r);
static void u128_stderr_print(u128 x);
static void test(int);
static u128 k, v, e;

int main(void) {
	k = U128(0x2B, 0x7E, 0x15, 0x16, 0x28, 0xAE, 0xD2, 0xA6, 0xAB, 0xF7, 0x15, 0x88, 0x09, 0xCF, 0x4F, 0x3C);
	v = U128(0x6B, 0xC1, 0xBE, 0xE2, 0x2E, 0x40, 0x9F, 0x96, 0xE9, 0x3D, 0x7E, 0x11, 0x73, 0x93, 0x17, 0x2A);
	e = U128(0x3A, 0xD7, 0x7B, 0xB4, 0x0D, 0x7A, 0x36, 0x60, 0xA8, 0x9E, 0xCA, 0xF3, 0x24, 0x66, 0xEF, 0x97);
	test(1);

	puts("All OK");
	return 0;
}

static void test(int n) {
	u128 g;
	aes_do_encrypt(&k, &v, &g);

	if(!u128_eq(e, g)) {
		fprintf(stderr, "Test %d failed!\n", n);
		fputs("Expected ", stderr);
		u128_stderr_print(e);
		fputs("\n     Got ", stderr);
		u128_stderr_print(g);
		fputs("\n", stderr);
		exit(n);
	}
}

static bool u128_eq(u128 l, u128 r) {
	return l.b0 == r.b0;
}

static void u128_stderr_print(u128 x) {
	fprintf(stderr, "%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x%02x",
		x.b0, x.b1, x.b2,  x.b3,  x.b4,  x.b5,  x.b6,  x.b7,
		x.b8, x.b9, x.b10, x.b11, x.b12, x.b13, x.b14, x.b15);
}
