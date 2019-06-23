#!/usr/bin/env python3

import random
import socket
import struct
import sys

assert len(sys.argv) > 2

NOTIFICATION = 0xcb832fc8958bf9e6

frame = struct.pack(
    '<B3Q',
    5,
    random.getrandbits(64),
    eval(sys.argv[1]),
    NOTIFICATION) + sys.argv[2].encode('utf-8')

sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.sendto(frame, ('127.0.0.1', 51441))
