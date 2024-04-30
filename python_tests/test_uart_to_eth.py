import struct
import unittest
import util
import socket

class TestUartToEth(unittest.TestCase):
    def testNFrames(self):
        """
        Sends a frame multiple times over UART, including preamble and a valid
        frame check sequence and tests that the frames are returned over ethernet.
        """
        N = 5
        frame = b"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffabcdefhijklmnopqrstuvwABCDEFHIJKLMNOPQRSTUVW0123456789\xa2\xba\xed\xe1"
        with util.open_socket() as sock, util.open_serial() as ser:
            ser.write(N * util.make_uart_packet(frame))
            for _ in range(N):
                response = sock.recv(1500)
                self.assertEqual(b"\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xffabcdefhijklmnopqrstuvwABCDEFHIJKLMNOPQRSTUVW0123456789", response)
