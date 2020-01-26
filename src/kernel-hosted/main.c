#include "args.h"
#include "vm.h"

#include <stdio.h>

#include "util.h"

int main(int argc, char** argv) {
	parse_args(argc, argv);
	buf init = read_file(args.init_path);
	struct vm* vm = new_vm_default(&init);

	free_vm(vm);
	return 0;
}
