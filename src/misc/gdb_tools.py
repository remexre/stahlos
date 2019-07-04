import gdb
import struct


def command(func):  # decorator
    class DecoratedCommand(gdb.Command):
        def __init__(self):
            name = func.__name__
            super(DecoratedCommand, self).__init__(name, gdb.COMMAND_DATA)

        def invoke(self, arg, from_tty):
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
            func(frame, stack, read)

    DecoratedCommand()


@command
def stack(frame, stack, read):
    out = '<{}>'.format(len(stack))
    while len(stack) != 0:
        out += ' {:x}'.format(stack.pop())
    print(out)


@command
def typeln(frame, stack, read):
    assert len(stack) >= 2
    print(read(stack[1], stack[0]))
