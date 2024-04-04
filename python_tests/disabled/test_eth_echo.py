import unittest
import util
import env

class TestEthEcho(unittest.TestCase):
    def testEthEcho(self):
        eth_type = b'\xff\xff'
        payload = "Hello, world!".encode('utf-8')
        padding_needed = 46 - len(payload)
        if padding_needed > 0:
            payload += b'\x00' * padding_needed

        with util.open_socket() as s:
            mac = util.get_mac_addr(s, env.IFNAME)
            frame = mac + mac + eth_type + payload
            s.send(frame)
            response = s.recv(1500)
            self.assertEqual(frame, response)
