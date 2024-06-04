import unittest
import os
import socket
import random

dst_ip = '192.168.1.123'

class TestArpUdpEcho(unittest.TestCase):
    def testArpUdpEcho(self):
        with socket.socket(socket.AF_INET, socket.SOCK_DGRAM) as s:
            s.settimeout(5)

            for _ in range(50):
                data = os.urandom(random.randint(0,1000))
                port = random.randint(0, 65535)
                s.sendto(data, (dst_ip, port))
                self.assertEqual((data, (dst_ip, port)), s.recvfrom(1500))