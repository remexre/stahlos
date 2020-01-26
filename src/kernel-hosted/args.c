#define STAHLOS__KERNEL_HOSTED__ARGS_C 1
#include "args.h"

#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>

struct {
	const char* init_path;
} args = {
	.init_path = NULL,
};

static noreturn void usage(const char* name);

void parse_args(int argc, char** argv) {
	const char* usage_name = argc ? argv[0] : "kernel.elf";

	int opt;
	while((opt = getopt(argc, argv, "")) != -1) {
		switch(opt) {
		default:
			usage(usage_name);
		}
	}

	if(argc - optind != 1)
		usage(usage_name);
	args.init_path = argv[optind];
}

static noreturn void usage(const char* name) {
	fprintf(stderr, "Usage: %s <init>\n", name);
	exit(EXIT_FAILURE);
}
