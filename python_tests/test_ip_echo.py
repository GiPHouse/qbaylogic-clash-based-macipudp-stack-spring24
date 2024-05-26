import unittest
import util
import os
import socket
import struct
import random

IFNAME = os.environ['IFNAME']
DEV    = os.environ['DEV']

def open_socket():
    s = socket.socket(socket.AF_PACKET, socket.SOCK_RAW, socket.htons(3))
    s.bind((IFNAME, 0))
    return s

src_ip = '192.168.1.0'
dst_ip = '192.168.1.123'
src_mac = b"\x8c\x8c\xaa\xc8\x2b\xee" # hardcoded in echo stack
dst_mac = b"\x00\x00\x00\xff\xff\xff"

class TestEthUartEcho(unittest.TestCase):
    def _test(self, dst_ip):
        """
        Sends five IP packets and tests if they are sent back
        """
        with open_socket() as s:
            s.settimeout(5)

            for _ in range(5):
                data = os.urandom(random.randint(0,25))
                payload = struct.pack("!HHHH", 1337, 1337, len(data) + 8, 0) + data
                ip_header = create_ip_header(src_ip, dst_ip, len(payload))
                packet = dst_mac + src_mac + b"\x08\x00" + ip_header + payload

                s.send(packet)

                response = s.recv(2**16 - 1)
                total_length = struct.unpack_from("!H", response, 16)[0]
                response = response[:total_length + 14]

                expected_ip_header = create_ip_header(dst_ip, src_ip, len(payload))
                expected = src_mac + dst_mac + b"\x08\x00" +  expected_ip_header + payload

                self.assertEqual(expected, response)

    def testWithIp(self):
        self._test(dst_ip)

    def testWithBroadcast(self):
        self._test("192.168.1.255")

def create_ip_header(src_ip, dst_ip, payload_length):
    header = bytearray(struct.pack('!BBHHHBBH4s4s',
                       (4 << 4) + 5, # version, ihl
                       0,
                       20 + payload_length, # total length
                       0,
                       0,
                       64, # TTL. Set to 64 so Wireshark thinks this is a totally normal UDP packet
                       0, # Protocol. 0, because we can't set this in ipLitePacketizerC
                       0, # checksum
                       socket.inet_aton(src_ip),
                       socket.inet_aton(dst_ip)))
    checksum = util.internet_checksum(header)
    header[10:12] = struct.pack("!H", checksum)
    return bytes(header)
