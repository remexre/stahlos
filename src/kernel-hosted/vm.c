#include "vm.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

struct vm {
	uint8_t* mem;
	size_t mem_size;
	uuid current_pid;
};

struct vm* new_vm(buf* init_src, size_t mem_size, uuid init_pid) {
	assert(mem_size < 0xff00000000000000);
	struct vm* vm = malloc(sizeof(*vm));
	vm->mem = calloc(mem_size, 1);
	vm->mem_size = mem_size;
	vm->current_pid = init_pid;

	vm_write_u64(vm, 8 * hash_pid(init_pid), 0x818);
	vm_write_u64(vm, 0x810, 0xc38 + init_src->len);
	vm_write_u64(vm, 0x818, 0x838);
	vm_write_u64(vm, 0x820, init_pid.lo);
	vm_write_u64(vm, 0x828, init_pid.hi);
	// TODO 0x838, where should predefined code go
	vm_write_u64(vm, 0x840, 0xc38);
	vm_write_u64(vm, 0x848, init_src->len);
	vm_write_buf(vm, 0xc38, init_src);

	return vm;
}

struct vm* new_vm_default(buf* init_src) {
	return new_vm(init_src, 256 * 1024 * 1024, uuid_new_v4());
}

void free_vm(struct vm* vm) {
	free(vm->mem);
	free(vm);
}

void vm_write_buf(struct vm* vm, size_t addr, buf* buf) {
	assert(addr < vm->mem_size);
	assert(addr + buf->len <= vm->mem_size);
	memcpy(&vm->mem[addr], buf->bytes, buf->len);
}

uint8_t vm_read_u8(struct vm* vm, size_t addr) {
	assert(addr < vm->mem_size);
	return vm->mem[addr];
}

void vm_write_u8(struct vm* vm, size_t addr, uint8_t val) {
	assert(addr < vm->mem_size);
	vm->mem[addr] = val;
}

#define VM_RW(TY, IDENT, HALF_TY, HALF_IDENT, HALF_SZ) \
	TY vm_read_##IDENT(struct vm* vm, size_t addr) { \
		return (((TY) vm_read_##HALF_IDENT(vm, addr + HALF_SZ)) << (HALF_SZ * 8)) \
			| ((TY) vm_read_##HALF_IDENT(vm, addr)); \
	} \
	void vm_write_##IDENT(struct vm* vm, size_t addr, TY val) { \
		vm_write_##HALF_IDENT(vm, addr, (HALF_TY) val); \
		vm_write_##HALF_IDENT(vm, addr + HALF_SZ, (HALF_TY) (val >> (HALF_SZ * 8))); \
	}

VM_RW(uint16_t, u16, uint8_t, u8, 1)
VM_RW(uint32_t, u32, uint16_t, u16, 2)
VM_RW(uint64_t, u64, uint32_t, u32, 4)
