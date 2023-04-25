#!/usr/bin/env python3

# This file can be used as a script to sent messages to a given mac address.
# Usage:                        sudo ./mac_frame.py [Dev] [Mac_Dest] [Message]
# Example (broadcast):          sudo ./mac_frame.py eno1 ff:ff:ff:ff:ff:ff 'Hello everybody!'
#
# Bind IP address to device:    sudo ip addr add [IP] dev [Dev]
# Example:                      sudo ip addr add 100.1.1.1 dev eno1

import fcntl
import socket
import struct
import sys

ifname = sys.argv[1]
dest_mac = bytes.fromhex(sys.argv[2].replace(':', ''))
eth_type = b'\x7A\x05' # Two bytes. (Arbitrary ?)
message = sys.argv[3]

payload = message.encode('utf-8')
assert len(payload) <= 1500 # Max 1500 bytes of payload.

with socket.socket(socket.AF_PACKET, socket.SOCK_RAW) as s:
    s.bind((ifname, 0)) # Bind it to the interface.

    # https://stackoverflow.com/questions/159137/getting-mac-address
    info = fcntl.ioctl(s.fileno(), 0x8927, struct.pack('256s', bytes(ifname, 'utf-8')[:15]))
    src_mac = bytes.fromhex(''.join('%02x' % b for b in info[18:24]))

    s.send(dest_mac + src_mac + eth_type + payload)

