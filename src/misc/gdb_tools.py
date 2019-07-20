from functools import *
import gdb
from math import *
import shutil
import string
import struct
import subprocess

ull = gdb.lookup_type('unsigned long long')
voidp = gdb.lookup_type('void').pointer()


def read(addr, length, fmt=None):
    inf = gdb.selected_inferior()
    mem = inf.read_memory(addr, length)
    if fmt is None:
        return mem.tobytes()
    else:
        return struct.unpack(fmt, mem)


def read_qword(addr):
    return read(addr, 8, 'Q')[0]


def str_bytes(bs):
    try:
        return bs.decode('utf-8')
    except UnicodeDecodeError:
        return bs


def read_str(addr, length):
    return str_bytes(read(addr, length))


def processes_unsorted():
    for i in range(0x10000):
        addr = read_qword(0x200000 + i * 8)
        while addr != 0:
            pid = read_qword(addr + 8)
            proc_addr = read_qword(addr + 16)
            name = read_str(
                read_qword(proc_addr + 0x60),
                read_qword(proc_addr + 0x68))
            yield (pid, proc_addr, name)
            addr = read_qword(addr)


def processes():
    ps = list(processes_unsorted())
    ps.sort(key=lambda x: x[0])
    return ps


def run_queue():
    def prev(addr): return read_qword(addr)

    def next(addr): return read_qword(addr + 8)

    def area(addr): return read_qword(addr + 0x10)

    ipb_addr = read_qword(0x200000 - 8)
    first_link_addr = read_qword(ipb_addr + 0x40)
    addr = first_link_addr
    prev_addr = None
    while True:
        area_addr = area(addr)
        pid = read_qword(area_addr)
        yield (pid, area_addr, addr)
        if prev_addr is not None:
            if prev_addr != prev(addr):
                print('Inconsistent prev for PID 0x{:016x}: 0x{:016x} vs 0x{:016x}'.format(
                    pid, prev_addr, prev(addr)))

        prev_addr = addr
        addr = next(addr)
        if addr == first_link_addr:
            break


def command(func):  # decorator
    class DecoratedCommand(gdb.Command):
        def __init__(self):
            name = func.__name__
            super(DecoratedCommand, self).__init__(name, gdb.COMMAND_DATA)

        def invoke(self, arg, from_tty):
            args = gdb.string_to_argv(arg)
            frame = gdb.selected_frame()
            func_name = frame.name()

            rsp = int(frame.read_register('rsp').cast(ull))
            r13 = int(frame.read_register('r13').cast(ull))

            # If we're in the int3 handler, skip its stack entries.
            if func_name == 'bp_handler':
                rsp += 48

            stack_size = (r13 - rsp) // 8
            stack = []
            if stack_size > 0:
                stack.append(int(frame.read_register('rbx').cast(ull)))
            for i in range(0, stack_size - 1):
                stack.append(read_qword(rsp + 8 * i))
            func(args, frame, stack)

    DecoratedCommand()
    return func


@command
def fn(args, frame, stack):  # forth next
    stop = False
    while True:
        gdb.execute('stepi')
        frame = gdb.selected_frame()
        rip = frame.read_register('rip')
        if stop:
            break
        if read(rip, 2) == b'\xff\xe0':
            stop = True
    if '+' in str(rip) or '<' not in str(rip):
        gdb.execute('disas {},+64'.format(int(rip)))
    else:
        gdb.execute('disas')


@command
def hd(args, frame, stack):
    assert len(args) == 1 or len(args) == 2
    addr = gdb.parse_and_eval(args[0]).cast(voidp)

    PRINTABLE = string.ascii_letters + string.punctuation + string.digits + ' '
    ROWS = 8 if len(args) == 1 else int(gdb.parse_and_eval(args[1]))

    bs = read(addr, ROWS * 16, '{}s'.format(ROWS * 16))[0]

    print('Dumping from 0x{:016x}\n'.format(int(addr)))
    if shutil.which('hexyl') is not None:
        p = subprocess.run('hexyl', input=bs)
        if p.returncode == 0:
            return

    out = ''
    for row in range(ROWS):
        out += '{:016x}  '.format(int(addr) + row * 16)
        for col in range(8):
            out += '{:02x} '.format(bs[row * 16 + col])
        for col in range(8):
            out += ' {:02x}'.format(bs[row * 16 + col + 8])
        out += '  '
        for col in range(8):
            ch = chr(bs[row * 16 + col])
            out += ch if ch in PRINTABLE else '.'
        out += ' '
        for col in range(8):
            ch = chr(bs[row * 16 + col + 8])
            out += ch if ch in PRINTABLE else '.'
        out += '\n'
    print(out)


@command
def ipb(args, frame, stack):
    assert len(args) == 0
    addr = read_qword(0x200000 - 8)
    rows = int((0x200000 - addr) / 16)
    hd([str(addr), str(rows)], frame, stack)


@command
def ph(args, frame, stack):
    assert len(args) > 0
    print(hex(gdb.parse_and_eval(' '.join(args)).cast(ull)))


@command
def procs(args, frame, stack):
    assert len(args) == 0
    r15 = int(frame.read_register('r15').cast(ull))

    rq = list(run_queue())
    next_pid = rq[0][0] if len(rq) == 1 else rq[1][0]
    last_pid = rq[-1][0]

    for (pid, addr, name) in processes():
        flags = ''
        flags += '*' if addr == r15 else ' '
        flags += 'N' if pid == next_pid else ' '
        flags += 'L' if pid == last_pid else ' '
        print('{} 0x{:016x} @ 0x{:016x}'.format(flags, pid, addr), end='')
        if name != b'':
            print(': {}'.format(name))
        else:
            print()


@command
def rstack(args, frame, stack):
    assert len(args) == 0

    rbp = int(frame.read_register('rbp').cast(ull))
    r14 = int(frame.read_register('r14').cast(ull))

    rstack = []
    for i in range((r14 - rbp) // 8):
        addr = rbp + 8 * i
        rstack.append(read_qword(addr))
    out = '<{}>'.format(len(rstack))
    while len(rstack) != 0:
        out += ' {:x}'.format(rstack.pop())
    print(out)


@command
def rq(args, frame, stack):
    assert len(args) == 0

    for (pid, area_addr, link_addr) in run_queue():
        print('0x{:016x} @ 0x{:016x} (via 0x{:016x})'.format(
            pid, area_addr, link_addr))


@command
def stack(args, frame, stack):
    assert len(args) == 0
    out = '<{}>'.format(len(stack))
    while len(stack) != 0:
        out += ' {:x}'.format(stack.pop())
    print(out)


@command
def typeln(args, frame, stack):
    assert len(args) == 0
    assert len(stack) >= 2
    print(read_str(stack[1], stack[0]))
