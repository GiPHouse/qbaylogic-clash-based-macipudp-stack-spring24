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

def get_mac_addr(socket, ifname):
    ifname_c = struct.pack('256s', ifname[:15].encode())
    info = fcntl.ioctl(socket.fileno(), 0x8927, ifname_c)
    return bytes.fromhex(info[18:24].hex())

ifname = sys.argv[1]
eth_type = b'\xff\xff'
message = sys.argv[2]

payload = message.encode('utf-8')
# pad payload to minimum ethernet length
padding_needed = 46 - len(payload)
if padding_needed > 0:
    payload += b'\x00' * padding_needed

assert len(payload) <= 1500

ETH_P_ALL=3
with socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL)) as s:
    s.bind((ifname, 0)) # Bind it to the interface.
    mac = get_mac_addr(s, ifname)
    # Send frame to self
    print(mac + mac + eth_type + payload)
    s.send(mac + mac + eth_type + payload)

    reponse = s.recv(1500)
    print(f"received response: \"{reponse}\"")
