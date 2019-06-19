#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>

extern uint64_t to_number(uint64_t default_base_hex, uint64_t ignored1, uint64_t ignored2,
	size_t length, char* addr, bool* ok);

typedef struct {
	char* str;
	bool hex;
	uint64_t expected;
	bool expok;
} test_case;
static int do_tests(test_case** tests);
static test_case* test_ok(bool hex, char* str, uint64_t expected);
static test_case* test_fail(bool hex, char* str);

int main(void) {
	uint64_t max_i64 = 0x7fffffffffffffff;
	uint64_t min_i64 = 0x8000000000000000;

	return do_tests((test_case*[]) {
		test_fail(true, ""), // 1
		test_fail(false, ""),

		test_ok(false, "0",   0), // 3
		test_ok(false, "-0",  0),
		test_ok(false, "#0",  0),
		test_ok(false, "#-0", 0),
		test_ok(false, "$0",  0),
		test_ok(false, "$-0", 0),
		test_ok(true,  "0",   0),
		test_ok(true,  "-0",  0),
		test_ok(true,  "#0",  0),
		test_ok(true,  "#-0", 0),
		test_ok(true,  "$0",  0),
		test_ok(true,  "$-0", 0),

		test_ok(false, "1",     1), // 15
		test_ok(false, "12",    12),
		test_ok(false, "123",   123),
		test_ok(false, "-123",  -123),
		test_ok(false, "#123",  123),
		test_ok(false, "$123",  0x123),
		test_ok(false, "#-123", -123),
		test_ok(false, "$-123", -0x123),
		test_ok(true,  "123",   0x123),
		test_ok(true,  "#123",  123),
		test_ok(true,  "$123",  0x123),
		test_ok(true,  "#-123", -123),
		test_ok(true,  "$-123", -0x123),

		test_ok(false, "9223372036854775807", max_i64), // 28
		test_ok(false, "-9223372036854775808", min_i64),
		test_ok(false, "#9223372036854775807", max_i64),
		test_ok(false, "#-9223372036854775808", min_i64),
		test_ok(true, "#9223372036854775807", max_i64),
		test_ok(true, "#-9223372036854775808", min_i64),

		test_ok(false, "100",  100), // 34
		test_ok(false, "$100", 0x100),

		test_fail(false, "123r5"), // 36
		test_fail(true, "123r5"),

		test_ok(false, "$abcdef", 0xabcdef), // 38
		test_ok(true, "abcdef", 0xabcdef),

		test_fail(false, "-"), // 40
		test_fail(false, "#-"),

		test_fail(false, "a"), // 42
		test_ok(true, "a", 0xa),
		test_ok(true, "AaA", 0xaaa),

		NULL
	});

	return 0;
}

static int do_tests(test_case** tests) {
	size_t i = 0;
	int failing = 0;
	test_case* test;
	while((test = tests[i++])) {
		uint64_t base = test->hex ? 16 : 10;
		bool this_failed = false;
		size_t len = strlen(test->str);

		printf("test %lu (\"%s\", %s)... ", i, test->str, test->hex ? "hex" : "dec");
		fflush(stdout);

		bool ok;
		uint64_t num = to_number(base, 0, 0, len, test->str, &ok);

		if(test->expok && !ok) {
			printf("failed, success expected\n");
			failing++;
		} else if(test->expok && num != test->expected) {
			printf("got wrong result; got %ld, expected %ld\n", num, test->expected);
			failing++;
		} else if(!test->expok && ok) {
			printf("succeeded (got %ld), failure expected\n", num);
			failing++;
		} else {
			printf("ok\n");
		}

		if(this_failed)
			failing++;
	}
	return failing;
}

static test_case* test_ok(bool hex, char* str, uint64_t expected) {
	test_case* test = malloc(sizeof(test_case));
	*test = (test_case) {
		.str = str,
		.hex = hex,
		.expected = expected,
		.expok = true,
	};
	return test;
}

static test_case* test_fail(bool hex, char* str) {
	test_case* test = malloc(sizeof(test_case));
	*test = (test_case) {
		.str = str,
		.hex = hex,
		.expected = 0,
		.expok = false,
	};
	return test;
}
