#ifndef STAHLOS__KERNEL_HOSTED__ARGS_H
#define STAHLOS__KERNEL_HOSTED__ARGS_H 1

#ifndef STAHLOS__KERNEL_HOSTED__ARGS_C
extern struct {
	const char* init_path;
} args;
#endif

void parse_args(int argc, char** argv);

#endif
