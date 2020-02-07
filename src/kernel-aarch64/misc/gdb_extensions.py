import gdb
import struct



u64 = gdb.lookup_type('unsigned long long')
voidp = gdb.lookup_type('void').pointer()


def read(addr, length, fmt=None):
    inf = gdb.selected_inferior()
    mem = inf.read_memory(addr, length)
    if fmt is None:
        return mem.tobytes()
    else:
        return struct.unpack(fmt, mem)


def read_u64(addr):
    return read(addr, 8, 'Q')[0]

def read_reg(frame, name):
    return int(frame.read_register(name).cast(u64))

def read_stack(frame, reg_top, reg_base, reg_depth):
    top = read_reg(frame, reg_top)
    base = read_reg(frame, reg_base)
    depth = read_reg(frame, reg_depth)

    if depth == 0:
        return []

    l = []
    for i in range(1, depth // 8):
        l.append(read_u64(base + i * 8))
    l.append(top)
    return l


def command(func):  # decorator
    class DecoratedCommand(gdb.Command):
        def __init__(self):
            name = func.__name__
            super(DecoratedCommand, self).__init__(name, gdb.COMMAND_DATA)

        def invoke(self, arg, from_tty):
            args = gdb.string_to_argv(arg)
            frame = gdb.selected_frame()
            func(args, frame, read_stack(frame, 'x10', 'x11', 'x12'),
                    read_stack(frame, 'x14', 'x15', 'x16'))

    return DecoratedCommand()



@command
def proctbl(args, frame, stack, rstack):
    assert len(args) == 0
    ptp = read_reg(frame, 'x19')
    print('DictPtr: 0x{:016x}'.format(read_u64(ptp)))
    print('SrcPtr:  0x{:016x}'.format(read_u64(ptp + 8)))
    print('SrcLen:  0x{:016x}'.format(read_u64(ptp + 16)))
    print('SrcOff:  0x{:016x}'.format(read_u64(ptp + 24)))
    print('SavedIP: 0x{:016x}'.format(read_u64(ptp + 32)))
    print('SavedSD: 0x{:016x}'.format(read_u64(ptp + 40)))
    print('SavedRD: 0x{:016x}'.format(read_u64(ptp + 48)))
    print('Mailbox: 0x{:016x}'.format(read_u64(ptp + 56)))

@command
def stack(args, frame, stack, rstack):
    assert len(args) == 0
    print('stack  [{}]'.format(', '.join('0x{:016x}'.format(x) for x in stack)))
    print('rstack [{}]'.format(', '.join('0x{:016x}'.format(x) for x in rstack)))
