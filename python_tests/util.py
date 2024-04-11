import socket
import fcntl
import struct
import serial
import os

IFNAME = os.environ['IFNAME']
DEV    = os.environ['DEV']

# baudrate needs to be the same as in TopEntity.hs
def open_serial():
    return serial.Serial(DEV, 115200, timeout=5)

def open_socket():
    ETH_P_ALL = 3
    s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL))
    s.bind((IFNAME, 0))
    return s

def get_mac_addr(socket, ifname=IFNAME):
    ifname_c = struct.pack('256s', ifname[:15].encode())
    info = fcntl.ioctl(socket.fileno(), 0x8927, ifname_c)
    return bytes.fromhex(info[18:24].hex())

def make_uart_packet(data: bytes):
    """
    Turns bytes into packets, to be read on the FPGA with toPacketC, uartRxC' or uartRxNoBaudGenC'
    """
    if data == b'':
        return b''
    return struct.pack("<H", len(data) - 1) + data
