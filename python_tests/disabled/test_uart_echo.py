import unittest
import util

class TestUartEcho(unittest.TestCase):
    def testUartEcho(self):
        with util.open_serial() as s:
            s.write(b'Hello, world!')
            self.assertEqual(b'Hello, world!', s.read(13))
