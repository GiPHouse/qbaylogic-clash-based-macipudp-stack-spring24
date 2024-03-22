import socket
import fnctl
import struct
import sys

import env

def get_mac_addr(socket, ifname=env.IFNAME):
    ifname_c = struct.pack('256s', ifname[:15].encode())
    info = fcntl.ioctl(socket.fileno(), 0x8927, ifname_c)
    return bytes.fromhex(info[18:24].hex())

def send_payload(message, ifname=env.IFNAME):
    eth_type = b'\xff\xff'
    payload = message.encode('utf-8')
    padding_needed = 46 - len(payload)
    if padding_needed > 0:
        payload += b'\x00' * padding_needed

    assert len(payload) <= 1500

    ETH_P_ALL = 3
    with socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL)) as s:
        s.bind((ifname, 0))
        mac = get_mac_addr(s, ifname)
        frame = mac + mac + eth_type + payload
        s.send(frame)
        response = s.recv(1500)
        return response
