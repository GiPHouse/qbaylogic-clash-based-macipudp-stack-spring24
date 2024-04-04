import struct
import unittest
import util
import env
import socket

class TestUartToEth(unittest.TestCase):
    def testUartToEth(self):
        eth_type = b'\xff\xff'
        payload = "The quick brown fox jumps over the lazy dog".encode('utf-8')
        padding_needed = 46 - len(payload)
        if padding_needed > 0:
            payload += b'\x00' * padding_needed

        with util.open_socket() as sock, util.open_serial() as ser:
            mac = util.get_mac_addr(sock, env.IFNAME)
            frame = mac + mac + eth_type + payload

            ser.write(util.make_uart_packet(frame))
            response = sock.recv(1500)
            self.assertEqual(frame, response)
