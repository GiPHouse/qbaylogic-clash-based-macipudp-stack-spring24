#!/usr/bin/env python3

# This file can be used as a script to send an ethernet frame and read it via UART
# Usage:                        sudo ./eth_uart.py [If] [Dev] [Message]
# Example (broadcast):          sudo ./eth_uart.py eno1 /dev/ttyACM0 "Hello, world!"

import sys
import socket
import fcntl
import struct
import serial

def get_mac_addr(socket, ifname):
    ifname_c = struct.pack('256s', ifname[:15].encode())
    info = fcntl.ioctl(socket.fileno(), 0x8927, ifname_c)
    return bytes.fromhex(info[18:24].hex())

ifname = sys.argv[1]
eth_type = b'\xff\xff'
dev = sys.argv[2]
message = sys.argv[3]

payload = message.encode('utf-8')
# pad payload to minimum ethernet length
padding_needed = 46 - len(payload)
if padding_needed > 0:
    payload += b'\x00' * padding_needed

assert len(payload) <= 1500

ETH_P_ALL=3
with socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL)) as so, serial.Serial(dev, 115200, timeout=5) as se:
    so.bind((ifname, 0)) # Bind it to the interface.
    mac = get_mac_addr(so, ifname)
    # Send frame to self
    print("Transmitting frame:")
    frame = mac + mac + eth_type + payload
    print(frame)
    so.send(frame)

    reponse = se.read(1500)
    print("Received response:")
    print(reponse)
