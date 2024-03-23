import socket
import fcntl
import struct
import sys
import serial

import env

def open_serial():
    return serial.Serial(env.DEV, 19200, timeout=5)

def open_socket():
    ETH_P_ALL = 3
    s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL))
    s.bind((env.IFNAME, 0))
    return s

def get_mac_addr(socket, ifname=env.IFNAME):
    ifname_c = struct.pack('256s', ifname[:15].encode())
    info = fcntl.ioctl(socket.fileno(), 0x8927, ifname_c)
    return bytes.fromhex(info[18:24].hex())
