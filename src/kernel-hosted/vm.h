#ifndef STAHLOS__KERNEL_HOSTED__VM_H
#define STAHLOS__KERNEL_HOSTED__VM_H 1

#include "util.h"

#include <stddef.h>
#include <stdint.h>

struct vm;

struct vm* new_vm(buf* init_src, size_t mem_size, uuid init_pid);
struct vm* new_vm_default(buf* init_src);
void free_vm(struct vm*);

uint8_t vm_read_u8(struct vm*, size_t addr);
uint16_t vm_read_u16(struct vm*, size_t addr);
uint32_t vm_read_u32(struct vm*, size_t addr);
uint64_t vm_read_u64(struct vm*, size_t addr);

void vm_write_u8(struct vm*, size_t addr, uint8_t val);
void vm_write_u16(struct vm*, size_t addr, uint16_t val);
void vm_write_u32(struct vm*, size_t addr, uint32_t val);
void vm_write_u64(struct vm*, size_t addr, uint64_t val);
void vm_write_buf(struct vm*, size_t addr, buf* buf);

#endif
