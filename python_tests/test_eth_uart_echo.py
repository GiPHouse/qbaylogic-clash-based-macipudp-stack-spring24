import unittest
import util

input = "Hello, world!"

class TestEthUartEcho(unittest.TestCase):
    def testEthUartEcho(self):
        eth_type = b'\xff\xff'
        payload = input.encode('utf-8')
        padding_needed = 46 - len(payload)
        payload += b'\x00' * padding_needed

        with util.open_socket() as s, util.open_serial() as se:
            mac = util.get_mac_addr(s, util.IFNAME)
            frame = mac + mac + eth_type + payload
            s.send(frame)
            response = se.read(1500)
            self.assertTrue(payload in response,
                            msg=f'\n\nExpected: response contains\n{payload}\n\nActual response: \n{response}')
