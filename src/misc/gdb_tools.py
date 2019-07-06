import gdb
import string
import struct


def command(func):  # decorator
    class DecoratedCommand(gdb.Command):
        def __init__(self):
            name = func.__name__
            super(DecoratedCommand, self).__init__(name, gdb.COMMAND_DATA)

        def invoke(self, arg, from_tty):
            args = gdb.string_to_argv(arg)
            inf = gdb.selected_inferior()
            frame = gdb.selected_frame()
            func_name = frame.name()

            def read(addr, length, fmt=None):
                mem = inf.read_memory(addr, length)
                if fmt is None:
                    return mem.tobytes()
                else:
                    return struct.unpack(fmt, mem)

            rsp = int(frame.read_register('rsp'))
            r13 = int(frame.read_register('r13'))

            # If we're in the int3 handler, skip its stack entries.
            if func_name == 'bp_handler':
                rsp += 40

            stack_size = (r13 - rsp) // 8
            stack = []
            if stack_size > 0:
                stack.append(int(frame.read_register('rbx')))
            for i in range(0, stack_size - 1):
                stack.append(read(rsp + 8 * i, 8, 'Q')[0])
            func(args, frame, stack, read)

    DecoratedCommand()


@command
def hd(args, frame, stack, read):
    assert len(args) == 1
    addr = gdb.parse_and_eval(args[0]).cast(gdb.lookup_type('void').pointer())

    PRINTABLE = string.ascii_letters + string.punctuation + string.digits + ' '
    ROWS = 8

    bs = read(addr, ROWS * 16, '{}s'.format(ROWS * 16))[0]
    out = ''
    for row in range(ROWS):
        out += '{:08x}  '.format(int(addr) + row * 16)
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
def nf(args, frame, stack, read):  # next forth
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
def rstack(args, frame, stack, read):
    assert len(args) == 0

    rbp = int(frame.read_register('rbp'))
    r14 = int(frame.read_register('r14'))

    rstack = []
    for i in range((r14 - rbp) // 8):
        addr = rbp + 8 * i
        rstack.append(read(addr, 8, 'Q')[0])
    out = '<{}>'.format(len(rstack))
    while len(rstack) != 0:
        out += ' {:x}'.format(rstack.pop())
    print(out)


@command
def stack(args, frame, stack, read):
    assert len(args) == 0
    out = '<{}>'.format(len(stack))
    while len(stack) != 0:
        out += ' {:x}'.format(stack.pop())
    print(out)


@command
def typeln(args, frame, stack, read):
    assert len(args) == 0
    assert len(stack) >= 2
    print(read(stack[1], stack[0]))
