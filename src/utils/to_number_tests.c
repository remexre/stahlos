#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdnoreturn.h>

typedef /* noreturn */ void(*continuation)(void);
extern noreturn uint64_t to_number(continuation next, uint64_t ignored,
	char* addr, size_t length, uint64_t* out);

static noreturn void do_tests(void);
typedef struct {
	char* str;
	uint64_t expected;
	bool expok;
} test_case;
static test_case* test_ok(char* str, uint64_t expected);
static test_case* test_fail(char* str);
static test_case** test_cases;

int main(void) {
	test_cases = (test_case*[]) {
		NULL
	};
	do_tests();
}

static noreturn void do_tests(void) {
	static size_t i = 0;
	static uint64_t out = 0;

	if(!test_cases[i])
		exit(0);
	assert("that I'll write this soon");
	exit(1);
}

static void add_test_ok(char* str, uint64_t expected);

static void add_test_fail(char* str);
