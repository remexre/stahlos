import gdb
import shutil
import string
import struct
import subprocess


u64 = gdb.lookup_type('unsigned long long')
voidp = gdb.lookup_type('void').pointer()


def read(addr, length, fmt=None):
    inf = gdb.selected_inferior()
    mem = inf.read_memory(addr, length)
    if fmt is None:
        return mem.tobytes()
    else:
        return struct.unpack(fmt, mem)


def read_u8(addr):
    return read(addr, 1, 'b')[0]


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
def hd(args, frame, stack, rstack):
    assert len(args) == 1 or len(args) == 2
    addr = gdb.parse_and_eval(args[0]).cast(voidp)

    PRINTABLE = string.ascii_letters + string.punctuation + string.digits + ' '
    ROWS = 8 if len(args) == 1 else int(gdb.parse_and_eval(args[1]))

    bs = read(addr, ROWS * 16, '{}s'.format(ROWS * 16))[0]

    print('Dumping from 0x{:016x}\n'.format(int(addr)))
    if shutil.which('hexyl') is not None:
        p = subprocess.run(['hexyl'], input=bs, stdout=subprocess.PIPE)
        if p.returncode == 0:
            sys.stdout.write(p.stdout.decode('utf-8'))
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
def proctbl(args, frame, stack, rstack):
    assert len(args) == 0
    ptp = read_reg(frame, 'x19')
    print('DictPtr: 0x{:016x}'.format(read_u64(ptp)))
    print('SrcPtr:  0x{:016x}'.format(read_u64(ptp + 8)))
    print('SrcLen:  0x{:016x}'.format(read_u64(ptp + 16)))
    print('SrcOff:  0x{:016x}'.format(read_u64(ptp + 24)))
    print('Mailbox: 0x{:016x}'.format(read_u64(ptp + 32)))

    flag_bits = read_u64(ptp + 40)
    flags = []
    flags.append('Interpret' if (flag_bits & 1) != 0 else 'Compile')
    print('Flags:   {}'.format(', '.join(flags)))

    print('SavedIP: 0x{:016x}'.format(read_u64(ptp + 64)))
    print('SavedSD: 0x{:016x}'.format(read_u64(ptp + 72)))
    print('SavedRD: 0x{:016x}'.format(read_u64(ptp + 80)))


@command
def stack(args, frame, stack, rstack):
    assert len(args) == 0
    print(
        'stack  [{}]'.format(
            ', '.join('0x{:016x}'.format(x) for x in stack)))
    print(
        'rstack [{}]'.format(
            ', '.join('0x{:016x}'.format(x) for x in rstack)))


@command
def words(args, frame, stack, rstack):
    assert len(args) == 0
    ptp = read_reg(frame, 'x19')
    header = read_u64(ptp)
    while header != 0:
        next_header = read_u64(header)
        flags = read_u8(header + 8)
        name_len = read_u8(header + 9)
        name = read(header + 10, name_len).decode('utf-8')
        cfa = header + 10 + name_len
        print('0x{:016x} {: <16} cfa=0x{:016x}'.format(header, name, cfa))
        header = next_header
